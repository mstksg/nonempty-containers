{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns       #-}

-- |
-- Module      : Data.IntMap.NonEmpty.Internal
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Unsafe internal-use functions used in the implementation of
-- "Data.IntMap.NonEmpty".  These functions can potentially be used to
-- break the abstraction of 'NEIntMap' and produce unsound sets, so be
-- wary!
module Data.IntMap.NonEmpty.Internal (
  -- * Non-Empty IntMap type
    NEIntMap(..)
  , Key
  , singleton
  , nonEmptyMap
  , fromList
  , toList
  , map
  , insertWith
  , union
  , unions
  , elems
  , size
  , toMap
  -- * Folds
  , foldr
  , foldr'
  , foldr1
  , foldl
  , foldl'
  , foldl1
  -- * Traversals
  , traverseWithKey
  , traverseWithKey1
  , foldMapWithKey
  , traverseMapWithKey
  -- * Unsafe IntMap Functions
  , insertMinMap
  , insertMaxMap
  -- * Debug
  , valid
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Coerce
import           Data.Data
import           Data.Function
import           Data.Functor.Apply
import           Data.Functor.Classes
import           Data.IntMap.Internal       (IntMap(..), Key)
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Foldable    (Foldable1(fold1))
import           Data.Semigroup.Traversable (Traversable1(..))
import           Data.Typeable              (Typeable)
import           Prelude hiding             (foldr1, foldl1, foldr, foldl, map)
import           Text.Read
import qualified Data.Foldable              as F
import qualified Data.IntMap                as M
import qualified Data.Semigroup.Foldable    as F1

-- | A non-empty IntMap from keys @k@ to values @a@.  At least one key-value
-- pair exists in an @'NEIntMap' k v@ at all times.
--
-- Functions that /take/ an 'NEIntMap' can safely operate on it with the
-- assumption that it has at least one key-value pair.
--
-- Functions that /return/ an 'NEIntMap' provide an assurance that the result
-- has at least one key-value pair.
--
-- "Data.IntMap.NonEmpty" re-exports the API of "Data.IntMap", faithfully
-- reproducing asymptotics, typeclass constraints, and semantics.
-- Functions that ensure that input and output maps are both non-empty
-- (like 'Data.IntMap.NonEmpty.insert') return 'NEIntMap', but functions that
-- might potentially return an empty map (like 'Data.IntMap.NonEmpty.delete')
-- return a 'IntMap' instead.
--
-- You can directly construct an 'NEIntMap' with the API from
-- "Data.IntMap.NonEmpty"; it's more or less the same as constructing a normal
-- 'IntMap', except you don't have access to 'Data.IntMap.empty'.  There are also
-- a few ways to construct an 'NEIntMap' from a 'IntMap':
--
-- 1.  The 'nonEmptyMap' smart constructor will convert a @'IntMap' k a@ into
--     a @'Maybe' ('NEIntMap' k a)@, returning 'Nothing' if the original 'IntMap'
--     was empty.
-- 2.  You can use the 'Data.IntMap.NonEmpty.insertIntMap' family of functions to
--     insert a value into a 'IntMap' to create a guarunteed 'NEIntMap'.
-- 3.  You can use the 'Data.IntMap.NonEmpty.IsNonEmpty' and
--     'Data.IntMap.NonEmpty.IsEmpty' patterns to "pattern match" on a 'IntMap'
--     to reveal it as either containing a 'NEIntMap' or an empty map.
-- 4.  'Data.IntMap.NonEmpty.withNonEmpty' offers a continuation-based interface
--     for deconstructing a 'IntMap' and treating it as if it were an 'NEIntMap'.
--
-- You can convert an 'NEIntMap' into a 'IntMap' with 'toMap' or
-- 'Data.IntMap.NonEmpty.IsNonEmpty', essentially "obscuring" the non-empty
-- property from the type.
data NEIntMap a =
    NEIntMap { neimK0     :: !Key    -- ^ invariant: must be smaller than smallest key in map
             , neimV0     :: a
             , neimIntMap :: !(IntMap a)
             }
  deriving (Typeable)

instance Eq a => Eq (NEIntMap a) where
    t1 == t2 = M.size (neimIntMap t1) == M.size (neimIntMap t2)
            && toList t1 == toList t2

instance Ord a => Ord (NEIntMap a) where
    compare = compare `on` toList
    (<)     = (<) `on` toList
    (>)     = (>) `on` toList
    (<=)    = (<=) `on` toList
    (>=)    = (>=) `on` toList

instance Eq1 NEIntMap where
    liftEq eq m1 m2 = M.size (neimIntMap m1) == M.size (neimIntMap m2)
                   && liftEq (liftEq eq) (toList m1) (toList m2)

instance Ord1 NEIntMap where
    liftCompare cmp m n =
        liftCompare (liftCompare cmp) (toList m) (toList n)

instance Show1 NEIntMap where
    liftShowsPrec sp sl d m =
        showsUnaryWith (liftShowsPrec sp' sl') "fromList" d (toList m)
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance Read1 NEIntMap where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance Read e => Read (NEIntMap e) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      xs <- parens . prec 10 $ readPrec
      return (fromList xs)
    readListPrec = readListPrecDefault

instance Show a => Show (NEIntMap a) where
    showsPrec d m  = showParen (d > 10) $
      showString "fromList (" . shows (toList m) . showString ")"

instance NFData a => NFData (NEIntMap a) where
    rnf (NEIntMap k v a) = rnf k `seq` rnf v `seq` rnf a

-- Data instance code from Data.IntMap.Internal
--
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
--                (c) wren romano 2016
instance Data a => Data (NEIntMap a) where
  gfoldl f z im = z fromList `f` (toList im)
  toConstr _     = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _   = intMapDataType
  dataCast1 f    = gcast1 f

fromListConstr :: Constr
fromListConstr = mkConstr intMapDataType "fromList" [] Prefix

intMapDataType :: DataType
intMapDataType = mkDataType "Data.IntMap.NonEmpty.Internal.NEIntMap" [fromListConstr]

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- > elemsList map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList ((5,"a") :| [(3,"bbb")])) == 4
foldr :: (a -> b -> b) -> b -> NEIntMap a -> b
foldr f z (NEIntMap _ v m) = v `f` M.foldr f z m
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> NEIntMap a -> b
foldr' f z (NEIntMap _ v m) = v `f` y
  where
    !y = M.foldr' f z m
{-# INLINE foldr' #-}

-- | /O(n)/. A version of 'foldr' that uses the value at the maximal key in
-- the map as the starting value.
--
-- Note that, unlike 'Data.Foldable.foldr1' for 'IntMap', this function is
-- total if the input function is total.
foldr1 :: (a -> a -> a) -> NEIntMap a -> a
foldr1 f (NEIntMap _ v m) = maybe v (f v . uncurry (M.foldr f))
                       . M.maxView
                       $ m
{-# INLINE foldr1 #-}

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- > elemsList = reverse . foldl (flip (:)) []
--
-- > let f len a = len + (length a)
-- > foldl f 0 (fromList ((5,"a") :| [(3,"bbb")])) == 4
foldl :: (a -> b -> a) -> a -> NEIntMap b -> a
foldl f z (NEIntMap _ v m) = M.foldl f (f z v) m
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> NEIntMap b -> a
foldl' f z (NEIntMap _ v m) = M.foldl' f x m
  where
    !x = f z v
{-# INLINE foldl' #-}

-- | /O(n)/. A version of 'foldl' that uses the value at the minimal key in
-- the map as the starting value.
--
-- Note that, unlike 'Data.Foldable.foldl1' for 'IntMap', this function is
-- total if the input function is total.
foldl1 :: (a -> a -> a) -> NEIntMap a -> a
foldl1 f (NEIntMap _ v m) = M.foldl f v m
{-# INLINE foldl1 #-}

-- | /O(n)/. Fold the keys and values in the map using the given semigroup,
-- such that
--
-- @'foldMapWithKey' f = 'Data.Semigroup.Foldable.fold1' . 'Data.IntMap.NonEmpty.mapWithKey' f@
--
-- __WARNING__: Differs from @Data.IntMap.foldMapWithKey@, which traverses
-- positive items first, then negative items.
--
-- This can be an asymptotically faster than
-- 'Data.IntMap.NonEmpty.foldrWithKey' or 'Data.IntMap.NonEmpty.foldlWithKey' for
-- some monoids.

-- TODO: benchmark against maxView method
foldMapWithKey
    :: Semigroup m
    => (Key -> a -> m)
    -> NEIntMap a
    -> m
foldMapWithKey f = F1.foldMap1 (uncurry f) . toList
{-# INLINE foldMapWithKey #-}

-- | /O(n)/. IntMap a function over all values in the map.
--
-- > map (++ "x") (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "bx") :| [(5, "ax")])
map :: (a -> b) -> NEIntMap a -> NEIntMap b
map f (NEIntMap k0 v m) = NEIntMap k0 (f v) (M.map f m)
{-# NOINLINE [1] map #-}
{-# RULES
"map/map" forall f g xs . map f (map g xs) = map (f . g) xs
 #-}
{-# RULES
"map/coerce" map coerce = coerce
 #-}

-- | /O(m*log(n\/m + 1)), m <= n/.
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and
-- @t2@. It prefers @t1@ when duplicate keys are encountered, i.e.
-- (@'union' == 'Data.IntMap.NonEmpty.unionWith' 'const'@).
--
-- > union (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == fromList ((3, "b") :| [(5, "a"), (7, "C")])
union
    :: NEIntMap a
    -> NEIntMap a
    -> NEIntMap a
union n1@(NEIntMap k1 v1 m1) n2@(NEIntMap k2 v2 m2) = case compare k1 k2 of
    LT -> NEIntMap k1 v1 . M.union m1 . toMap $ n2
    EQ -> NEIntMap k1 v1 . M.union m1         $ m2
    GT -> NEIntMap k2 v2 . M.union (toMap n1) $ m2
{-# INLINE union #-}

-- | The left-biased union of a non-empty list of maps.
--
-- > unions (fromList ((5, "a") :| [(3, "b")]) :| [fromList ((5, "A") :| [(7, "C")]), fromList ((5, "A3") :| [(3, "B3")])])
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions (fromList ((5, "A3") :| [(3, "B3")]) :| [fromList ((5, "A") :| [(7, "C")]), fromList ((5, "a") :| [(3, "b")])])
-- >     == fromList ((3, "B3") :| [(5, "A3"), (7, "C")])
unions
    :: Foldable1 f
    => f (NEIntMap a)
    -> NEIntMap a
unions (F1.toNonEmpty->(m :| ms)) = F.foldl' union m ms
{-# INLINE unions #-}

-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
--
-- > elems (fromList ((5,"a") :| [(3,"b")])) == ("b" :| ["a"])
elems :: NEIntMap a -> NonEmpty a
elems (NEIntMap _ v m) = v :| M.elems m
{-# INLINE elems #-}

-- | /O(1)/. The number of elements in the map.  Guaranteed to be greater
-- than zero.
--
-- > size (singleton 1 'a')                          == 1
-- > size (fromList ((1,'a') :| [(2,'c'), (3,'b')])) == 3
size :: NEIntMap a -> Int
size (NEIntMap _ _ m) = 1 + M.size m
{-# INLINE size #-}

-- | /O(log n)/.
-- Convert a non-empty map back into a normal possibly-empty map, for usage
-- with functions that expect 'IntMap'.
--
-- Can be thought of as "obscuring" the non-emptiness of the map in its
-- type.  See the 'Data.IntMap.NonEmpty.IsNotEmpty' pattern.
--
-- 'nonEmptyMap' and @'maybe' 'Data.IntMap.empty' 'toMap'@ form an isomorphism: they
-- are perfect structure-preserving inverses of eachother.
--
-- > toMap (fromList ((3,"a") :| [(5,"b")])) == Data.IntMap.fromList [(3,"a"), (5,"b")]
toMap :: NEIntMap a -> IntMap a
toMap (NEIntMap k v m) = insertMinMap k v m
{-# INLINE toMap #-}

-- | /O(n)/.
-- @'traverseWithKey' f m == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- /Use 'traverseWithKey1'/ whenever possible (if your 'Applicative'
-- also has 'Apply' instance).  This version is provided only for types
-- that do not have 'Apply' instance, since 'Apply' is not at the moment
-- (and might not ever be) an official superclass of 'Applicative'.
--
-- __WARNING__: Differs from @Data.IntMap.traverseWithKey@, which traverses
-- positive items first, then negative items.
--
-- @
-- 'traverseWithKey' f = 'unwrapApplicative' . 'traverseWithKey1' (\\k -> WrapApplicative . f k)
-- @
traverseWithKey
    :: Applicative t
    => (Key -> a -> t b)
    -> NEIntMap a
    -> t (NEIntMap b)
traverseWithKey f (NEIntMap k v m0) =
        NEIntMap k <$> f k v
                   <*> traverseMapWithKey f m0
{-# INLINE traverseWithKey #-}

-- | /O(n)/.
-- @'traverseWithKey1' f m == 'fromList' <$> 'traverse1' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
--
-- That is, behaves exactly like a regular 'traverse1' except that the traversing
-- function also has access to the key associated with a value.
--
-- __WARNING__: Differs from @Data.IntMap.traverseWithKey@, which traverses
-- positive items first, then negative items.
--
-- Is more general than 'traverseWithKey', since works with all 'Apply',
-- and not just 'Applicative'.

-- TODO: benchmark against maxView-based methods
traverseWithKey1
    :: Apply t
    => (Key -> a -> t b)
    -> NEIntMap a
    -> t (NEIntMap b)
traverseWithKey1 f (NEIntMap k0 v m0) = case runMaybeApply m1 of
    Left  m2 -> NEIntMap k0 <$> f k0 v <.> m2
    Right m2 -> flip (NEIntMap k0) m2 <$> f k0 v
  where
    m1 = traverseMapWithKey (\k -> MaybeApply . Left . f k) m0
{-# INLINABLE traverseWithKey1 #-}

-- | /O(n)/. Convert the map to a non-empty list of key\/value pairs.
--
-- > toList (fromList ((5,"a") :| [(3,"b")])) == ((3,"b") :| [(5,"a")])
toList :: NEIntMap a -> NonEmpty (Key, a)
toList (NEIntMap k v m) = (k,v) :| M.toList m
{-# INLINE toList #-}

-- | /O(log n)/. Smart constructor for an 'NEIntMap' from a 'IntMap'.  Returns
-- 'Nothing' if the 'IntMap' was originally actually empty, and @'Just' n@
-- with an 'NEIntMap', if the 'IntMap' was not empty.
--
-- 'nonEmptyMap' and @'maybe' 'Data.IntMap.empty' 'toMap'@ form an
-- isomorphism: they are perfect structure-preserving inverses of
-- eachother.
--
-- See 'Data.IntMap.NonEmpty.IsNonEmpty' for a pattern synonym that lets you
-- "match on" the possiblity of a 'IntMap' being an 'NEIntMap'.
--
-- > nonEmptyMap (Data.IntMap.fromList [(3,"a"), (5,"b")]) == fromList ((3,"a") :| [(5,"b")])
nonEmptyMap :: IntMap a -> Maybe (NEIntMap a)
nonEmptyMap = (fmap . uncurry . uncurry) NEIntMap . M.minViewWithKey
{-# INLINE nonEmptyMap #-}

-- | /O(n*log n)/. Build a non-empty map from a non-empty list of
-- key\/value pairs. See also 'Data.IntMap.NonEmpty.fromAscList'. If the list
-- contains more than one value for the same key, the last value for the
-- key is retained.
--
-- > fromList ((5,"a") :| [(3,"b"), (5, "c")]) == fromList ((5,"c") :| [(3,"b")])
-- > fromList ((5,"c") :| [(3,"b"), (5, "a")]) == fromList ((5,"a") :| [(3,"b")])

-- TODO: write manually and optimize to be equivalent to
-- 'fromDistinctAscList' if items are ordered, just like the actual
-- 'M.fromList'.
fromList :: NonEmpty (Key, a) -> NEIntMap a
fromList ((k, v) :| xs) = maybe (singleton k v) (insertWith (const id) k v)
                        . nonEmptyMap
                        $ M.fromList xs
{-# INLINE fromList #-}

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList ((1, 'a') :| [])
-- > size (singleton 1 'a') == 1
singleton :: Key -> a -> NEIntMap a
singleton k v = NEIntMap k v M.empty
{-# INLINE singleton #-}

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@ will insert the pair (key, value) into
-- @mp@ if key does not exist in the map. If the key does exist, the
-- function will insert the pair @(key, f new_value old_value)@.
--
-- See 'Data.IntMap.NonEmpty.insertIntMapWith' for a version where the first
-- argument is a 'IntMap'.
--
-- > insertWith (++) 5 "xxx" (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "xxxa")])
-- > insertWith (++) 7 "xxx" (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "a"), (7, "xxx")])
insertWith
    :: (a -> a -> a)
    -> Key
    -> a
    -> NEIntMap a
    -> NEIntMap a
insertWith f k v n@(NEIntMap k0 v0 m) = case compare k k0 of
    LT -> NEIntMap k  v        . toMap            $ n
    EQ -> NEIntMap k  (f v v0) m
    GT -> NEIntMap k0 v0       $ M.insertWith f k v m
{-# INLINE insertWith #-}


-- | Left-biased union
instance Semigroup (NEIntMap a) where
    (<>) = union
    {-# INLINE (<>) #-}
    sconcat = unions
    {-# INLINE sconcat #-}

instance Functor NEIntMap where
    fmap = map
    {-# INLINE fmap #-}
    x <$ NEIntMap k _ m = NEIntMap k x (x <$ m)
    {-# INLINE (<$) #-}

-- | Traverses elements in order of ascending keys.
--
-- __WARNING:__ 'fold' and 'foldMap' are different than for the 'IntMap'
-- instance.  They traverse elements in order of ascending keys, while
-- 'IntMap' traverses positive keys first, then negative keys.
--
-- 'Data.Foldable.foldr1', 'Data.Foldable.foldl1', 'Data.Foldable.minimum',
-- 'Data.Foldable.maximum' are all total.
instance Foldable NEIntMap where
    fold      (NEIntMap _ v m) = v <> F.fold (M.elems m)
    {-# INLINE fold #-}
    foldMap f (NEIntMap _ v m) = f v <> foldMap f (M.elems m)
    {-# INLINE foldMap #-}
    foldr   = foldr
    {-# INLINE foldr #-}
    foldr'  = foldr'
    {-# INLINE foldr' #-}
    foldr1  = foldr1
    {-# INLINE foldr1 #-}
    foldl   = foldl
    {-# INLINE foldl #-}
    foldl'  = foldl'
    {-# INLINE foldl' #-}
    foldl1  = foldl1
    {-# INLINE foldl1 #-}
    null _  = False
    {-# INLINE null #-}
    length  = size
    {-# INLINE length #-}
    elem x (NEIntMap _ v m) = F.elem x m
                           || x == v
    {-# INLINE elem #-}
    toList  = F.toList . elems
    {-# INLINE toList #-}

-- | Traverses elements in order of ascending keys
--
-- __WARNING:__ Different than for the 'IntMap' instance.  They traverse
-- elements in order of ascending keys, while 'IntMap' traverses positive
-- keys first, then negative keys.
instance Traversable NEIntMap where
    traverse f = traverseWithKey (const f)
    {-# INLINE traverse #-}

-- | Traverses elements in order of ascending keys
--
-- __WARNING:__ 'fold1' and 'foldMap1' are different than 'fold' and
-- 'foldMap' for the 'IntMap' instance of 'Foldable'.  They traverse
-- elements in order of ascending keys, while 'IntMap' traverses positive
-- keys first, then negative keys.
instance Foldable1 NEIntMap where
    fold1 (NEIntMap _ v m) = maybe v (v <>)
                           . getOption
                           . F.foldMap (Option . Just)
                           . M.elems
                           $ m
    {-# INLINE fold1 #-}
    foldMap1 f = foldMapWithKey (const f)
    {-# INLINE foldMap1 #-}
    toNonEmpty = elems
    {-# INLINE toNonEmpty #-}

-- | Traverses elements in order of ascending keys
--
-- __WARNING:__ 'traverse1' and 'sequence1' are different 'traverse' and
-- 'sequence' for the 'IntMap' instance of 'Traversable'.  They traverse
-- elements in order of ascending keys, while 'IntMap' traverses positive
-- keys first, then negative keys.
instance Traversable1 NEIntMap where
    traverse1 f = traverseWithKey1 (const f)
    {-# INLINE traverse1 #-}

-- | /O(n)/. Test if the internal map structure is valid.
valid :: NEIntMap a -> Bool
valid (NEIntMap k _ m) = all ((k <) . fst . fst) (M.minViewWithKey m)





-- | /O(log n)/. Insert new key and value into a map where keys are
-- /strictly greater than/ the new key.  That is, the new key must be
-- /strictly less than/ all keys present in the 'IntMap'.  /The precondition
-- is not checked./
--
-- At the moment this is simply an alias for @Data.IntSet.insert@, but it's
-- left here as a placeholder in case this eventually gets implemented in
-- a more efficient way.

-- TODO: implementation
insertMinMap :: Key -> a -> IntMap a -> IntMap a
insertMinMap = M.insert
{-# INLINABLE insertMinMap #-}

-- | /O(log n)/. Insert new key and value into a map where keys are
-- /strictly less than/ the new key.  That is, the new key must be
-- /strictly greater than/ all keys present in the 'IntMap'.  /The
-- precondition is not checked./
--
-- At the moment this is simply an alias for @Data.IntSet.insert@, but it's
-- left here as a placeholder in case this eventually gets implemented in
-- a more efficient way.

-- TODO: implementation
insertMaxMap :: Key -> a -> IntMap a -> IntMap a
insertMaxMap = M.insert
{-# INLINABLE insertMaxMap #-}

-- | /O(n)/. A fixed version of 'Data.IntMap.traverseWithKey' that
-- traverses items in ascending order of keys.
traverseMapWithKey :: Applicative t => (Key -> a -> t b) -> IntMap a -> t (IntMap b)
traverseMapWithKey f = go
  where
    go Nil = pure Nil
    go (Tip k v) = Tip k <$> f k v
    go (Bin p m l r) = liftA2 (flip (Bin p m)) (go r) (go l)
{-# INLINE traverseMapWithKey #-}

