{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE ViewPatterns       #-}
{-# OPTIONS_HADDOCK not-home    #-}

-- |
-- Module      : Data.Set.NonEmpty.Internal
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Unsafe internal-use functions used in the implementation of
-- "Data.Set.NonEmpty".  These functions can potentially be used to break
-- the abstraction of 'NESet' and produce unsound sets, so be wary!
module Data.Set.NonEmpty.Internal (
    NESet(..)
  , nonEmptySet
  , withNonEmpty
  , toSet
  , singleton
  , fromList
  , toList
  , size
  , union
  , unions
  , foldr
  , foldl
  , foldr'
  , foldl'
  , MergeNESet(..)
  , merge
  , valid
  , insertMinSet
  , insertMaxSet
  , disjointSet
  , powerSetSet
  , disjointUnionSet
  , cartesianProductSet
  ) where

import           Control.DeepSeq
import           Control.Monad
import           Data.Data
import           Data.Function
import           Data.Functor.Classes
import           Data.List.NonEmpty      (NonEmpty(..))
import           Data.Semigroup
import           Data.Semigroup.Foldable (Foldable1)
import           Data.Set.Internal       (Set(..))
import           Prelude hiding          (foldr, foldr1, foldl, foldl1)
import           Text.Read
import qualified Data.Aeson              as A
import qualified Data.Foldable           as F
import qualified Data.Semigroup.Foldable as F1
import qualified Data.Set                as S
import qualified Data.Set.Internal       as S

#if !MIN_VERSION_containers(0,5,11)
import           Utils.Containers.Internal.StrictPair
#endif

-- | A non-empty (by construction) set of values @a@.  At least one value
-- exists in an @'NESet' a@ at all times.
--
-- Functions that /take/ an 'NESet' can safely operate on it with the
-- assumption that it has at least one item.
--
-- Functions that /return/ an 'NESet' provide an assurance that the result
-- has at least one item.
--
-- "Data.Set.NonEmpty" re-exports the API of "Data.Set", faithfully
-- reproducing asymptotics, typeclass constraints, and semantics.
-- Functions that ensure that input and output sets are both non-empty
-- (like 'Data.Set.NonEmpty.insert') return 'NESet', but functions that
-- might potentially return an empty map (like 'Data.Set.NonEmpty.delete')
-- return a 'Set' instead.
--
-- You can directly construct an 'NESet' with the API from
-- "Data.Set.NonEmpty"; it's more or less the same as constructing a normal
-- 'Set', except you don't have access to 'Data.Set.empty'.  There are also
-- a few ways to construct an 'NESet' from a 'Set':
--
-- 1.  The 'nonEmptySet' smart constructor will convert a @'Set' a@ into
--     a @'Maybe' ('NESet' a)@, returning 'Nothing' if the original 'Set'
--     was empty.
-- 2.  You can use the 'Data.Set.NonEmpty.insertSet' family of functions to
--     insert a value into a 'Set' to create a guaranteed 'NESet'.
-- 3.  You can use the 'Data.Set.NonEmpty.IsNonEmpty' and
--     'Data.Set.NonEmpty.IsEmpty' patterns to "pattern match" on a 'Set'
--     to reveal it as either containing a 'NESet' or an empty map.
-- 4.  'withNonEmpty' offers a continuation-based interface for
--     deconstructing a 'Set' and treating it as if it were an 'NESet'.
--
-- You can convert an 'NESet' into a 'Set' with 'toSet' or
-- 'Data.Set.NonEmpty.IsNonEmpty', essentially "obscuring" the non-empty
-- property from the type.
data NESet a =
    NESet { nesV0  :: !a   -- ^ invariant: must be smaller than smallest value in set
          , nesSet :: !(Set a)
          }
  deriving (Typeable)

instance Eq a => Eq (NESet a) where
    t1 == t2  = S.size (nesSet t1) == S.size (nesSet t2)
             && toList t1 == toList t2

instance Ord a => Ord (NESet a) where
    compare = compare `on` toList
    (<)     = (<) `on` toList
    (>)     = (>) `on` toList
    (<=)    = (<=) `on` toList
    (>=)    = (>=) `on` toList

instance Show a => Show (NESet a) where
    showsPrec p xs = showParen (p > 10) $
      showString "fromList (" . shows (toList xs) . showString ")"

instance (Read a, Ord a) => Read (NESet a) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      xs <- parens . prec 10 $ readPrec
      return (fromList xs)

    readListPrec = readListPrecDefault

instance Eq1 NESet where
    liftEq eq m n =
        size m == size n && liftEq eq (toList m) (toList n)

instance Ord1 NESet where
    liftCompare cmp m n =
        liftCompare cmp (toList m) (toList n)

instance Show1 NESet where
    liftShowsPrec sp sl d m =
        showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)

instance NFData a => NFData (NESet a) where
    rnf (NESet x s) = rnf x `seq` rnf s

-- Data instance code from Data.Set.Internal
--
-- Copyright   :  (c) Daan Leijen 2002
instance (Data a, Ord a) => Data (NESet a) where
  gfoldl f z set = z fromList `f` toList set
  toConstr _     = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _   = setDataType
  dataCast1 f    = gcast1 f

fromListConstr :: Constr
fromListConstr = mkConstr setDataType "fromList" [] Prefix

setDataType :: DataType
setDataType = mkDataType "Data.Set.NonEmpty.Internal.NESet" [fromListConstr]


instance A.ToJSON a => A.ToJSON (NESet a) where
    toJSON     = A.toJSON . toSet
    toEncoding = A.toEncoding . toSet

instance (A.FromJSON a, Ord a) => A.FromJSON (NESet a) where
    parseJSON = withNonEmpty (fail err) pure
            <=< A.parseJSON
      where
        err = "NESet: Non-empty set expected, but empty set found"


-- | /O(log n)/. Smart constructor for an 'NESet' from a 'Set'.  Returns
-- 'Nothing' if the 'Set' was originally actually empty, and @'Just' n@
-- with an 'NESet', if the 'Set' was not empty.
--
-- 'nonEmptySet' and @'maybe' 'Data.Set.empty' 'toSet'@ form an
-- isomorphism: they are perfect structure-preserving inverses of
-- eachother.
--
-- See 'Data.Set.NonEmpty.IsNonEmpty' for a pattern synonym that lets you
-- "match on" the possiblity of a 'Set' being an 'NESet'.
--
-- > nonEmptySet (Data.Set.fromList [3,5]) == Just (fromList (3:|[5]))
nonEmptySet :: Set a -> Maybe (NESet a)
nonEmptySet = (fmap . uncurry) NESet . S.minView
{-# INLINE nonEmptySet #-}

-- | /O(log n)/. A general continuation-based way to consume a 'Set' as if
-- it were an 'NESet'. @'withNonEmpty' def f@ will take a 'Set'.  If set is
-- empty, it will evaluate to @def@.  Otherwise, a non-empty set 'NESet'
-- will be fed to the function @f@ instead.
--
-- @'nonEmptySet' == 'withNonEmpty' 'Nothing' 'Just'@
withNonEmpty
    :: r                  -- ^ value to return if set is empty
    -> (NESet a -> r)     -- ^ function to apply if set is not empty
    -> Set a
    -> r
withNonEmpty def f = maybe def f . nonEmptySet
{-# INLINE withNonEmpty #-}

-- | /O(log n)/.
-- Convert a non-empty set back into a normal possibly-empty map, for usage
-- with functions that expect 'Set'.
--
-- Can be thought of as "obscuring" the non-emptiness of the set in its
-- type.  See the 'Data.Set.NonEmpty.IsNotEmpty' pattern.
--
-- 'nonEmptySet' and @'maybe' 'Data.Set.empty' 'toSet'@ form an
-- isomorphism: they are perfect structure-preserving inverses of
-- eachother.
--
-- > toSet (fromList ((3,"a") :| [(5,"b")])) == Data.Set.fromList [(3,"a"), (5,"b")]
toSet :: NESet a -> Set a
toSet (NESet x s) = insertMinSet x s
{-# INLINE toSet #-}

-- | /O(1)/. Create a singleton set.
singleton :: a -> NESet a
singleton x = NESet x S.empty
{-# INLINE singleton #-}

-- | /O(n*log n)/. Create a set from a list of elements.

-- TODO: write manually and optimize to be equivalent to
-- 'fromDistinctAscList' if items are ordered, just like the actual
-- 'S.fromList'.
fromList :: Ord a => NonEmpty a -> NESet a
fromList (x :| s) = withNonEmpty (singleton x) (<> singleton x)
                  . S.fromList
                  $ s
{-# INLINE fromList #-}

-- | /O(n)/. Convert the set to a non-empty list of elements.
toList :: NESet a -> NonEmpty a
toList (NESet x s) = x :| S.toList s
{-# INLINE toList #-}

-- | /O(1)/. The number of elements in the set.  Guaranteed to be greater
-- than zero.
size :: NESet a -> Int
size (NESet _ s) = 1 + S.size s
{-# INLINE size #-}

-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'Data.Set.NonEmpty.toAscList'@.
--
-- For example,
--
-- > elemsList set = foldr (:) [] set
foldr :: (a -> b -> b) -> b -> NESet a -> b
foldr f z (NESet x s) = x `f` S.foldr f z s
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> NESet a -> b
foldr' f z (NESet x s) = x `f` y
  where
    !y = S.foldr' f z s
{-# INLINE foldr' #-}

-- | /O(n)/. A version of 'foldr' that uses the value at the maximal value
-- in the set as the starting value.
--
-- Note that, unlike 'Data.Foldable.foldr1' for 'Set', this function is
-- total if the input function is total.
foldr1 :: (a -> a -> a) -> NESet a -> a
foldr1 f (NESet x s) = maybe x (f x . uncurry (S.foldr f))
                     . S.maxView
                     $ s
{-# INLINE foldr1 #-}

-- | /O(n)/. Fold the elements in the set using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'Data.Set.NonEmpty.toAscList'@.
--
-- For example,
--
-- > descElemsList set = foldl (flip (:)) [] set
foldl :: (a -> b -> a) -> a -> NESet b -> a
foldl f z (NESet x s) = S.foldl f (f z x) s
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> NESet b -> a
foldl' f z (NESet x s) = S.foldl' f y s
  where
    !y = f z x
{-# INLINE foldl' #-}

-- | /O(n)/. A version of 'foldl' that uses the value at the minimal value
-- in the set as the starting value.
--
-- Note that, unlike 'Data.Foldable.foldl1' for 'Set', this function is
-- total if the input function is total.
foldl1 :: (a -> a -> a) -> NESet a -> a
foldl1 f (NESet x s) = S.foldl f x s
{-# INLINE foldl1 #-}

-- | /O(m*log(n\/m + 1)), m <= n/. The union of two sets, preferring the first set when
-- equal elements are encountered.
union
    :: Ord a
    => NESet a
    -> NESet a
    -> NESet a
union n1@(NESet x1 s1) n2@(NESet x2 s2) = case compare x1 x2 of
    LT -> NESet x1 . S.union s1 . toSet $ n2
    EQ -> NESet x1 . S.union s1         $ s2
    GT -> NESet x2 . S.union (toSet n1) $ s2
{-# INLINE union #-}

-- | The union of a non-empty list of sets
unions
    :: (Foldable1 f, Ord a)
    => f (NESet a)
    -> NESet a
unions (F1.toNonEmpty->(s :| ss)) = F.foldl' union s ss
{-# INLINE unions #-}

-- | Left-biased union
instance Ord a => Semigroup (NESet a) where
    (<>) = union
    {-# INLINE (<>) #-}
    sconcat = unions
    {-# INLINE sconcat #-}

-- | Traverses elements in ascending order
--
-- 'Data.Foldable.foldr1', 'Data.Foldable.foldl1', 'Data.Foldable.minimum',
-- 'Data.Foldable.maximum' are all total.
instance Foldable NESet where
#if MIN_VERSION_base(4,11,0)
    fold      (NESet x s) = x <> F.fold s
    {-# INLINE fold #-}
    foldMap f (NESet x s) = f x <> foldMap f s
    {-# INLINE foldMap #-}
#else
    fold      (NESet x s) = x `mappend` F.fold s
    {-# INLINE fold #-}
    foldMap f (NESet x s) = f x `mappend` foldMap f s
    {-# INLINE foldMap #-}
#endif
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
    elem x (NESet x0 s) = F.elem x s
                       || x == x0
    {-# INLINE elem #-}
    minimum (NESet x _) = x
    {-# INLINE minimum #-}
    maximum (NESet x s) = maybe x fst . S.maxView $ s
    {-# INLINE maximum #-}
    -- TODO: use build
    toList  = F.toList . toList
    {-# INLINE toList #-}

-- | Traverses elements in ascending order
instance Foldable1 NESet where
    fold1 (NESet x s) = option x (x <>)
                      . F.foldMap (Option . Just)
                      $ s
    {-# INLINE fold1 #-}
    -- TODO: benchmark against maxView-based method
    foldMap1 f (NESet x s) = option (f x) (f x <>)
                           . F.foldMap (Option . Just . f)
                           $ s
    {-# INLINE foldMap1 #-}
    toNonEmpty = toList
    {-# INLINE toNonEmpty #-}


-- | Used for 'Data.Set.NonEmpty.cartesianProduct'
newtype MergeNESet a = MergeNESet { getMergeNESet :: NESet a }

instance Semigroup (MergeNESet a) where
    MergeNESet n1 <> MergeNESet n2 = MergeNESet (merge n1 n2)
    {-# INLINE (<>) #-}

-- | Unsafely merge two disjoint sets.  Only legal if all items in the
-- first set are less than all items in the second set
merge :: NESet a -> NESet a -> NESet a
merge (NESet x1 s1) n2 = NESet x1 $ s1 `S.merge` toSet n2

-- | /O(n)/. Test if the internal set structure is valid.
valid :: Ord a => NESet a -> Bool
valid (NESet x s) = S.valid s
                  && all ((x <) . fst) (S.minView s)




-- | /O(log n)/. Insert new value into a set where values are
-- /strictly greater than/ the new values  That is, the new value must be
-- /strictly less than/ all values present in the 'Set'.  /The precondition
-- is not checked./
--
-- While this has the same asymptotics as @Data.Set.insert@, it saves
-- a constant factor for value comparison (so may be helpful if comparison
-- is expensive) and also does not require an 'Ord' instance for the value
-- type.
insertMinSet :: a -> Set a -> Set a
insertMinSet x = \case
    Tip         -> S.singleton x
    Bin _ y l r -> balanceL y (insertMinSet x l) r
{-# INLINABLE insertMinSet #-}

-- | /O(log n)/. Insert new value into a set where values are /strictly
-- less than/ the new value.  That is, the new value must be /strictly
-- greater than/ all values present in the 'Set'.  /The precondition is not
-- checked./
--
-- While this has the same asymptotics as @Data.Set.insert@, it saves
-- a constant factor for value comparison (so may be helpful if comparison
-- is expensive) and also does not require an 'Ord' instance for the value
-- type.
insertMaxSet :: a -> Set a -> Set a
insertMaxSet x = \case
    Tip         -> S.singleton x
    Bin _ y l r -> balanceR y l (insertMaxSet x r)
{-# INLINABLE insertMaxSet #-}

-- ---------------------------------------------
-- | CPP for new functions not in old containers
-- ---------------------------------------------

-- | Comptability layer for 'Data.Set.disjoint'.
disjointSet :: Ord a => Set a -> Set a -> Bool
#if MIN_VERSION_containers(0,5,11)
disjointSet = S.disjoint
#else
disjointSet xs = S.null . S.intersection xs
#endif
{-# INLINE disjointSet #-}

-- | Comptability layer for 'Data.Set.powerSet'.
powerSetSet :: Set a -> Set (Set a)
#if MIN_VERSION_containers(0,5,11)
powerSetSet = S.powerSet
{-# INLINE powerSetSet #-}
#else
powerSetSet xs0 = insertMinSet S.empty (S.foldr' step' Tip xs0) where
  step' x pxs = insertMinSet (S.singleton x) (insertMinSet x `S.mapMonotonic` pxs) `glue` pxs
{-# INLINABLE powerSetSet #-}

minViewSure :: a -> Set a -> Set a -> StrictPair a (Set a)
minViewSure = go
  where
    go x Tip r = x :*: r
    go x (Bin _ xl ll lr) r =
      case go xl ll lr of
        xm :*: l' -> xm :*: balanceR x l' r

maxViewSure :: a -> Set a -> Set a -> StrictPair a (Set a)
maxViewSure = go
  where
    go x l Tip = x :*: l
    go x l (Bin _ xr rl rr) =
      case go xr rl rr of
        xm :*: r' -> xm :*: balanceL x l r'

glue :: Set a -> Set a -> Set a
glue Tip r = r
glue l Tip = l
glue l@(Bin sl xl ll lr) r@(Bin sr xr rl rr)
  | sl > sr = let !(m :*: l') = maxViewSure xl ll lr in balanceR m l' r
  | otherwise = let !(m :*: r') = minViewSure xr rl rr in balanceL m l r'
#endif

-- | Comptability layer for 'Data.Set.disjointUnion'.
disjointUnionSet :: Set a -> Set b -> Set (Either a b)
#if MIN_VERSION_containers(0,5,11)
disjointUnionSet = S.disjointUnion
#else
disjointUnionSet as bs = S.merge (S.mapMonotonic Left as) (S.mapMonotonic Right bs)
#endif
{-# INLINE disjointUnionSet #-}

-- | Comptability layer for 'Data.Set.cartesianProduct'.
cartesianProductSet :: Set a -> Set b -> Set (a, b)
#if MIN_VERSION_containers(0,5,11)
cartesianProductSet = S.cartesianProduct
#else
cartesianProductSet as bs =
  getMergeSet $ foldMap (\a -> MergeSet $ S.mapMonotonic ((,) a) bs) as

newtype MergeSet a = MergeSet { getMergeSet :: Set a }

instance Semigroup (MergeSet a) where
    MergeSet xs <> MergeSet ys = MergeSet (S.merge xs ys)

instance Monoid (MergeSet a) where
    mempty = MergeSet S.empty
    mappend = (<>)
#endif
{-# INLINE cartesianProductSet #-}



-- ------------------------------------------
-- | Unexported code from "Data.Set.Internal"
-- ------------------------------------------

balanceR :: a -> Set a -> Set a -> Set a
balanceR x l r = case l of
    Tip -> case r of
      Tip -> Bin 1 x Tip Tip
      Bin _ _ Tip Tip -> Bin 2 x Tip r
      Bin _ rx Tip rr@Bin{} -> Bin 3 rx (Bin 1 x Tip Tip) rr
      Bin _ rx (Bin _ rlx _ _) Tip -> Bin 3 rlx (Bin 1 x Tip Tip) (Bin 1 rx Tip Tip)
      Bin rs rx rl@(Bin rls rlx rll rlr) rr@(Bin rrs _ _ _)
        | rls < ratio*rrs -> Bin (1+rs) rx (Bin (1+rls) x Tip rl) rr
        | otherwise -> Bin (1+rs) rlx (Bin (1+S.size rll) x Tip rll) (Bin (1+rrs+S.size rlr) rx rlr rr)
    Bin ls _ _ _ -> case r of
      Tip -> Bin (1+ls) x l Tip
      Bin rs rx rl rr
         | rs > delta*ls  -> case (rl, rr) of
              (Bin rls rlx rll rlr, Bin rrs _ _ _)
                | rls < ratio*rrs -> Bin (1+ls+rs) rx (Bin (1+ls+rls) x l rl) rr
                | otherwise -> Bin (1+ls+rs) rlx (Bin (1+ls+S.size rll) x l rll) (Bin (1+rrs+S.size rlr) rx rlr rr)
              (_, _) -> error "Failure in Data.Map.balanceR"
                | otherwise -> Bin (1+ls+rs) x l r
{-# NOINLINE balanceR #-}

balanceL :: a -> Set a -> Set a -> Set a
balanceL x l r = case r of
    Tip -> case l of
      Tip -> Bin 1 x Tip Tip
      Bin _ _ Tip Tip -> Bin 2 x l Tip
      Bin _ lx Tip (Bin _ lrx _ _) -> Bin 3 lrx (Bin 1 lx Tip Tip) (Bin 1 x Tip Tip)
      Bin _ lx ll@Bin{} Tip -> Bin 3 lx ll (Bin 1 x Tip Tip)
      Bin ls lx ll@(Bin lls _ _ _) lr@(Bin lrs lrx lrl lrr)
        | lrs < ratio*lls -> Bin (1+ls) lx ll (Bin (1+lrs) x lr Tip)
        | otherwise -> Bin (1+ls) lrx (Bin (1+lls+S.size lrl) lx ll lrl) (Bin (1+S.size lrr) x lrr Tip)
    Bin rs _ _ _ -> case l of
             Tip -> Bin (1+rs) x Tip r
             Bin ls lx ll lr
                | ls > delta*rs  -> case (ll, lr) of
                     (Bin lls _ _ _, Bin lrs lrx lrl lrr)
                       | lrs < ratio*lls -> Bin (1+ls+rs) lx ll (Bin (1+rs+lrs) x lr r)
                       | otherwise -> Bin (1+ls+rs) lrx (Bin (1+lls+S.size lrl) lx ll lrl) (Bin (1+rs+S.size lrr) x lrr r)
                     (_, _) -> error "Failure in Data.Set.NonEmpty.Internal.balanceL"
                | otherwise -> Bin (1+ls+rs) x l r
{-# NOINLINE balanceL #-}

delta,ratio :: Int
delta = 3
ratio = 2
