{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash          #-}
{-# LANGUAGE ViewPatterns       #-}

-- |
-- Module      : Data.Map.NonEmpty.Internal
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- This contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.


module Data.Map.NonEmpty.Internal (
  -- * Non-Empty Map type
    NEMap(..)
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
  , foldr1'
  , foldl
  , foldl'
  , foldl1
  , foldl1'
  -- * Traversals
  , traverseWithKey
  , traverseWithKey1
  , foldMapWithKey
  -- * Unsafe Map Functions
  , insertMinMap
  , insertMaxMap
  -- * Debug
  , valid
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Coerce
import           Data.Data                  (Data)
import           Data.Function
import           Data.Functor.Apply
import           Data.Functor.Classes
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Map                   (Map)
import           Data.Maybe hiding          (mapMaybe)
import           Data.Semigroup
import           Data.Semigroup.Foldable    (Foldable1(fold1))
import           Data.Semigroup.Traversable (Traversable1(..))
import           Data.Typeable              (Typeable)
import           GHC.Exts                   ( reallyUnsafePtrEquality#, isTrue# )
import           Prelude hiding             (lookup, foldr1, foldl1, foldr, foldl, filter, map)
import           Text.Read
import qualified Data.Foldable              as F
import qualified Data.Map                   as M
import qualified Data.Map.Internal          as M
import qualified Data.Semigroup.Foldable    as F1

-- | A non-empty Map from keys @k@ to values @a@.  At least one key-value
-- pair exists in an @'NEMap' k v@ at all times.
--
-- Functions that /take/ an 'NEMap' can safely operate on it with the
-- assumption that it has at least one key-value pair.
--
-- Functions that /return/ an 'NEMap' provide an assurance that the result
-- has at least one key-value pair.
--
-- "Data.Map.NonEmpty" re-exports the API of "Data.Map", faithfully
-- reproducing asymptotics, typeclass constraints, and semantics.
-- Functions that ensure that input and output maps are both non-empty
-- (like 'insert') return 'NEMap', but functions that might potentially
-- return an empty map (like 'delete') return a 'Map' instead.
--
-- You can directly construct an 'NEMap' with the API from
-- "Data.Map.NonEmpty"; it's more or less the same as constructing a normal
-- 'Map', except you don't have access to 'M.empty'.  There are also a few
-- ways to construct an 'NEMap' from a 'Map':
--
-- 1.  The 'nonEmptyMap' smart constructor will convert a @'Map' k a@ into
--     a @'Maybe' ('NEMap' k a)@, returning 'Nothing' if the original 'Map'
--     was empty.
-- 2.  You can use the 'insertMap' family of functions to insert a value
--     into a 'Map' to create a guarunteed 'NEMap'.
-- 3.  You can use the 'IsNonEmpty' and 'IsEmpty' patterns to "pattern
--     match" on a 'Map' to reveal it as either containing a 'NonEmpty' or an
--     empty map.
-- 4.  'withNEMap' offers a continuation-based interface for deconstructing
--     a 'Map' and treating it as if it were an 'NEMap'.
--
-- You can convert an 'NEMap' into a (possibly empty) 'Map' with 'toMap' or
-- 'IsNonEmpty', essentially "obscuring" the non-empty property from the
-- type.
data NEMap k a =
    NEMap { nemK0  :: !k   -- ^ invariant: must be smaller than smallest key in map
          , nemV0  :: a
          , nemMap :: !(Map k a)
          }
  deriving (Eq, Ord, Data, Typeable)

instance Eq2 NEMap where
    liftEq2 eqk eqv m n =
        size m == size n && liftEq (liftEq2 eqk eqv) (toList m) (toList n)

instance Eq k => Eq1 (NEMap k) where
    liftEq = liftEq2 (==)

instance Ord2 NEMap where
    liftCompare2 cmpk cmpv m n =
        liftCompare (liftCompare2 cmpk cmpv) (toList m) (toList n)

instance Ord k => Ord1 (NEMap k) where
    liftCompare = liftCompare2 compare

instance Show2 NEMap where
    liftShowsPrec2 spk slk spv slv d m =
        showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)
      where
        sp = liftShowsPrec2 spk slk spv slv
        sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (NEMap k) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Ord k, Read k) => Read1 (NEMap k) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Ord k, Read k, Read e) => Read (NEMap k e) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      xs <- readPrec
      return (fromList xs)
    readListPrec = readListPrecDefault

instance (Show k, Show a) => Show (NEMap k a) where
    showsPrec d m  = showParen (d > 10) $
      showString "fromList " . shows (toList m)

instance (NFData k, NFData a) => NFData (NEMap k a) where
    rnf (NEMap k v a) = rnf k `seq` rnf v `seq` rnf a

-- | /O(n)/. Fold the values in the map using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
--
-- > elemsList map = foldr (:) [] map
--
-- > let f a len = len + (length a)
-- > foldr f 0 (fromList ((5,"a") :| [(3,"bbb")])) == 4
foldr :: (a -> b -> b) -> b -> NEMap k a -> b
foldr f z (NEMap _ v m) = v `f` M.foldr f z m
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> NEMap k a -> b
foldr' f z (NEMap _ v m) = v `f` y
  where
    !y = M.foldr' f z m
{-# INLINE foldr' #-}

-- | /O(n)/. A version of 'foldr' that uses the value at the maximal key in
-- the map as the starting value.
--
-- Note that, unlike 'Data.Foldable.foldr1' for 'Map', this function is
-- total if the input function is total.
foldr1 :: (a -> a -> a) -> NEMap k a -> a
foldr1 f (NEMap _ v m) = maybe v (f v . uncurry (M.foldr f))
                       . M.maxView
                       $ m
{-# INLINE foldr1 #-}

-- | /O(n)/. A strict version of 'foldr1'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr1' :: (a -> a -> a) -> NEMap k a -> a
foldr1' f (NEMap _ v m) = case M.maxView m of
    Nothing      -> v
    Just (y, m') -> let !z = M.foldr' f y m' in v `f` z
{-# INLINE foldr1' #-}

-- | /O(n)/. Fold the values in the map using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
--
-- > elemsList = reverse . foldl (flip (:)) []
--
-- > let f len a = len + (length a)
-- > foldl f 0 (fromList ((5,"a") :| [(3,"bbb")])) == 4
foldl :: (a -> b -> a) -> a -> NEMap k b -> a
foldl f z (NEMap _ v m) = M.foldl f (f z v) m
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> NEMap k b -> a
foldl' f z (NEMap _ v m) = M.foldl' f x m
  where
    !x = f z v
{-# INLINE foldl' #-}

-- | /O(n)/. A version of 'foldl' that uses the value at the minimal key in
-- the map as the starting value.
--
-- Note that, unlike 'Data.Foldable.foldl1' for 'Map', this function is
-- total if the input function is total.
foldl1 :: (a -> a -> a) -> NEMap k a -> a
foldl1 f (NEMap _ v m) = M.foldl f v m
{-# INLINE foldl1 #-}

-- | /O(n)/. A strict version of 'foldl1'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl1' :: (a -> a -> a) -> NEMap k a -> a
foldl1' f (NEMap _ v m) = M.foldl' f v m
{-# INLINE foldl1' #-}

-- | /O(n)/. Fold the keys and values in the map using the given semigroup,
-- such that
--
-- @'foldMapWithKey' f = 'Data.Semigroup.Foldable.fold1' . 'mapWithKey' f@
--
-- This can be an asymptotically faster than 'foldrWithKey' or
-- 'foldlWithKey' for some monoids.

-- TODO: benchmark against maxView method
foldMapWithKey
    :: Semigroup m
    => (k -> a -> m)
    -> NEMap k a
    -> m
foldMapWithKey f (NEMap k0 v m) = maybe (f k0 v) (f k0 v <>)
                                . getOption
                                . M.foldMapWithKey (\k -> Option . Just . f k)
                                $ m
{-# INLINE foldMapWithKey #-}

-- | /O(n)/. Fold the values in the map using the given semigroup,
-- in ascending order.
foldMap1 :: Semigroup m => (a -> m) -> NEMap k a -> m
foldMap1 f = foldMapWithKey (const f)
{-# INLINE foldMap1 #-}

-- | /O(n)/. Map a function over all values in the map.
--
-- > map (++ "x") (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "bx") :| [(5, "ax")])
map :: (a -> b) -> NEMap k a -> NEMap k b
map f (NEMap k0 v m) = NEMap k0 (f v) (M.map f m)
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
-- (@'union' == 'unionWith' 'const'@).
--
-- > union (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == fromList ((3, "b") :| [(5, "a"), (7, "C")])
union
    :: Ord k
    => NEMap k a
    -> NEMap k a
    -> NEMap k a
union n1@(NEMap k1 v1 m1) n2@(NEMap k2 v2 m2) = case compare k1 k2 of
    LT -> NEMap k1 v1 . M.union m1 . toMap $ n2
    EQ -> NEMap k1 v1 . M.union m1 . toMap $ n2
    GT -> NEMap k2 v2 . M.union (toMap n1) $ m2
{-# INLINE union #-}

-- | The left-biased union of a non-empty list of maps.
--
-- > unions (fromList ((5, "a") :| [(3, "b")]) :| [fromList ((5, "A") :| [(7, "C")]), fromList ((5, "A3") :| [(3, "B3")])])
-- >     == fromList [(3, "b"), (5, "a"), (7, "C")]
-- > unions (fromList ((5, "A3") :| [(3, "B3")]) :| [fromList ((5, "A") :| [(7, "C")]), fromList ((5, "a") :| [(3, "b")])])
-- >     == fromList ((3, "B3") :| [(5, "A3"), (7, "C")])
unions
    :: (Foldable1 f, Ord k)
    => f (NEMap k a)
    -> NEMap k a
unions (F1.toNonEmpty->(m :| ms)) = F.foldl' union m ms
{-# INLINE unions #-}

-- | /O(n)/.
-- Return all elements of the map in the ascending order of their keys.
--
-- > elems (fromList ((5,"a") :| [(3,"b")])) == ("b" :| ["a"])
elems :: NEMap k a -> NonEmpty a
elems (NEMap _ v m) = v :| M.elems m
{-# INLINE elems #-}

-- | /O(1)/. The number of elements in the map.
--
-- > size (singleton 1 'a')                          == 1
-- > size (fromList ((1,'a') :| [(2,'c'), (3,'b')])) == 3
size :: NEMap k a -> Int
size (NEMap _ _ m) = 1 + M.size m
{-# INLINE size #-}

-- | /O(log n)/.
-- Convert a non-empty map back into a normal possibly-empty map, for usage
-- with functions that expect 'Map'.
--
-- Can be thought of as "obscuring" the non-emptiness of the map in its
-- type.  See the 'IsNotEmpty' pattern.
--
-- 'nonEmptyMap' and @'maybe' 'M.empty' 'toMap'@ form an isomorphism: they
-- are perfect structure-preserving inverses of eachother.
--
-- > toMap (fromList ((3,"a") :| [(5,"b")])) == Data.Map.fromList [(3,"a"), (5,"b")]
toMap :: NEMap k a -> Map k a
toMap (NEMap k v m) = insertMinMap k v m
{-# INLINE toMap #-}

-- | /O(n)/.
-- @'traverseWithKey' f m == 'fromNonEmpty' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- /Use 'traverseWithKey1'/ whenever possible (if your 'Applicative'
-- also has 'Apply' instance).  This version is provided only for types
-- that do not have 'Apply' instance, since 'Apply' is not at the moment
-- (and might not ever be) an official superclass of 'Applicative'.
--
-- @
-- 'traverseWithKey' f = 'unwrapApplicative' . 'traverseWithKey1' (\\k -> WrapApplicative . f k)
-- @
traverseWithKey
    :: Applicative t
    => (k -> a -> t b)
    -> NEMap k a
    -> t (NEMap k b)
traverseWithKey f (NEMap k v m0) = NEMap k <$> f k v <*> M.traverseWithKey f m0
{-# INLINE traverseWithKey #-}

-- | /O(n)/.
-- @'traverseWithKey1' f m == 'fromNonEmpty' <$> 'traverse1' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
--
-- That is, behaves exactly like a regular 'traverse1' except that the traversing
-- function also has access to the key associated with a value.
--
-- Is more general than 'traverseWithKey', since works with all 'Apply',
-- and not just 'Applicative'.

-- TODO: benchmark against maxView-based methods
traverseWithKey1
    :: Apply t
    => (k -> a -> t b)
    -> NEMap k a
    -> t (NEMap k b)
traverseWithKey1 f (NEMap k0 v m0) = case runMaybeApply m1 of
    Left  m2 -> NEMap k0 <$> f k0 v <.> m2
    Right m2 -> flip (NEMap k0) m2 <$> f k0 v
  where
    m1 = M.traverseWithKey (\k -> MaybeApply . Left . f k) m0
{-# INLINE traverseWithKey1 #-}

-- | /O(n)/. Convert the map to a non-empty list of key\/value pairs.
--
-- > toList (fromList ((5,"a") :| [(3,"b")])) == ((3,"b") :| [(5,"a")])
toList :: NEMap k a -> NonEmpty (k, a)
toList (NEMap k v m) = (k,v) :| M.toList m
{-# INLINE toList #-}

-- | /O(log n)/ Smart constructor for an 'NEMap' from a 'Map'.  Returns
-- 'Nothing' if the 'Map' was originally actually empty, and @'Just' n@
-- with an 'NEMap', if the 'Map' was not empty.
--
-- 'nonEmptyMap' and @'maybe' 'M.empty' 'toMap'@ form an isomorphism: they
-- are perfect structure-preserving inverses of eachother.
--
-- See 'IsNonEmpty' for a pattern synonym that lets you "match on" the
-- possiblity of a 'Map' being an 'NEMap'.
--
-- > nonEmptyMap (Data.Map.fromList [(3,"a"), (5,"b")]) == fromList ((3,"a") :| [(5,"b")])
nonEmptyMap :: Map k a -> Maybe (NEMap k a)
nonEmptyMap m = uncurry (\(k, v) -> NEMap k v) <$> M.minViewWithKey m
{-# INLINE nonEmptyMap #-}

-- | /O(n*log n)/. Build a non-empty map from a non-empty list of
-- key\/value pairs. See also 'fromAscList'. If the list contains more than
-- one value for the same key, the last value for the key is retained.
--
-- > fromList ((5,"a") :| [(3,"b"), (5, "c")]) == fromList ((5,"c") :| [(3,"b")])
-- > fromList ((5,"c") :| [(3,"b"), (5, "a")]) == fromList ((5,"a") :| [(3,"b")])

-- TODO: write manually and optimize to be equivalent to
-- 'fromDistinctAscList' if items are ordered, just like the actual
-- 'M.fromList'.
fromList :: Ord k => NonEmpty (k, a) -> NEMap k a
fromList ((k, v) :| xs) = maybe (singleton k v) (insertWith (const id) k v)
                        . nonEmptyMap
                        $ M.fromList xs
{-# INLINE fromList #-}

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList ((1, 'a') :| [])
-- > size (singleton 1 'a') == 1
singleton :: k -> a -> NEMap k a
singleton k v = NEMap k v M.empty
{-# INLINE singleton #-}

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@ will insert the pair (key, value) into
-- @mp@ if key does not exist in the map. If the key does exist, the
-- function will insert the pair @(key, f new_value old_value)@.
--
-- See 'insertMapWith' for a version where the first argument is a 'Map'.
--
-- > insertWith (++) 5 "xxx" (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "xxxa")])
-- > insertWith (++) 7 "xxx" (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "a"), (7, "xxx")])
insertWith
    :: Ord k
    => (a -> a -> a)
    -> k
    -> a
    -> NEMap k a
    -> NEMap k a
insertWith f k v n@(NEMap k0 v0 m) = case compare k k0 of
    LT -> NEMap k  v        . toMap            $ n
    EQ -> NEMap k  (f v v0) m
    GT -> NEMap k0 v0       $ M.insertWith f k v m
{-# INLINE insertWith #-}


-- | Left-biased union
instance Ord k => Semigroup (NEMap k a) where
    (<>) = union
    {-# INLINE (<>) #-}
    sconcat = unions
    {-# INLINE sconcat #-}

instance Functor (NEMap k) where
    fmap = map
    {-# INLINE fmap #-}
    x <$ NEMap k _ m = NEMap k x (x <$ m)
    {-# INLINE (<$) #-}

-- | Traverses elements in order ascending keys
--
-- 'foldr1', 'foldl1', 'minimum', 'maximum' are all total.
instance Foldable (NEMap k) where
    fold      (NEMap _ v m) = v <> F.fold m
    {-# INLINE fold #-}
    foldMap f (NEMap _ v m) = f v <> foldMap f m
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
    elem x (NEMap _ v m) = F.elem x m
                        || x == v
    {-# INLINE elem #-}
    minimum (NEMap _ v _) = v
    {-# INLINE minimum #-}
    maximum (NEMap _ v m) = maybe v snd . M.lookupMax $ m
    {-# INLINE maximum #-}
    toList  = F.toList . elems
    {-# INLINE toList #-}

-- | Traverses elements in order ascending keys
instance Traversable (NEMap k) where
    traverse f (NEMap k v m) = NEMap k <$> f v <*> traverse f m
    {-# INLINE traverse #-}
    sequenceA (NEMap k v m)  = NEMap k <$> v <*> sequenceA m
    {-# INLINE sequenceA #-}

-- | Traverses elements in order ascending keys
instance Foldable1 (NEMap k) where
    fold1 (NEMap _ v m) = maybe v (v <>)
                        . getOption
                        . F.foldMap (Option . Just)
                        $ m
    {-# INLINE fold1 #-}
    foldMap1   = foldMap1
    {-# INLINE foldMap1 #-}
    toNonEmpty = elems
    {-# INLINE toNonEmpty #-}

-- | Traverses elements in order ascending keys
instance Traversable1 (NEMap k) where
    traverse1 f = traverseWithKey1 (const f)
    {-# INLINE traverse1 #-}
    sequence1 (NEMap k v m0) = case runMaybeApply m1 of
        Left  m2 -> NEMap k <$> v <.> m2
        Right m2 -> flip (NEMap k) m2 <$> v
      where
        m1 = traverse (MaybeApply . Left) m0
    {-# INLINE sequence1 #-}

-- | /O(n)/. Test if the internal map structure is valid.
valid :: Ord k => NEMap k a -> Bool
valid (NEMap k _ m) = M.valid m
                   && all ((k <) . fst . fst) (M.minViewWithKey m)







-- | /O(log n)/. Insert new key and value into a map where keys are
-- /strictly greater than/ the new key.  That is, the new key must be
-- /strictly less than/ all keys present in the 'Map'.  /The precondition
-- is not checked./
--
-- While this has the same asymptotics as 'M.insert', it saves a constant
-- factor for key comparison (so may be helpful if comparison is
-- expensive) and also does not require an 'Ord' instance for the key type.
insertMinMap :: k -> a -> Map k a -> Map k a
insertMinMap kx0 = go kx0 kx0
  where
    go :: k -> k -> a -> Map k a -> Map k a
    go orig !_  x M.Tip = M.singleton (lazy orig) x
    go orig !kx x t@(M.Bin _ ky y l r)
        | l' `ptrEq` l = t
        | otherwise    = M.balanceL ky y l' r
      where
        !l' = go orig kx x l
{-# INLINABLE insertMinMap #-}

-- | /O(log n)/. Insert new key and value into a map where keys are
-- /strictly less than/ the new key.  That is, the new key must be
-- /strictly greater than/ all keys present in the 'Map'.  /The
-- precondition is not checked./
--
-- While this has the same asymptotics as 'M.insert', it saves a constant
-- factor for key comparison (so may be helpful if comparison is
-- expensive) and also does not require an 'Ord' instance for the key type.
insertMaxMap :: k -> a -> Map k a -> Map k a
insertMaxMap kx0 = go kx0 kx0
  where
    go :: k -> k -> a -> Map k a -> Map k a
    go orig !_  x M.Tip = M.singleton (lazy orig) x
    go orig !kx x t@(M.Bin _ ky y l r)
        | r' `ptrEq` r = t
        | otherwise    = M.balanceR ky y l r'
      where
        !r' = go orig kx x r
{-# INLINABLE insertMaxMap #-}

lazy :: a -> a
lazy x = x

ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
{-# INLINE ptrEq #-}
