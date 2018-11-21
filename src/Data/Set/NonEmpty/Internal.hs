{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash          #-}

module Data.Set.NonEmpty.Internal (
    NESet(..)
  , nonEmptySet
  , toSet
  , singleton
  , fromList
  , toList
  , size
  , union
  , valid
  , insertMinSet
  , insertMaxSet
  ) where

import           Control.DeepSeq
import           Data.Data
import           Data.Functor.Classes
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Set.Internal    (Set(..))
import           Data.Typeable        (Typeable)
import           GHC.Exts             ( reallyUnsafePtrEquality#, isTrue# )
import           Text.Read
import qualified Data.List.NonEmpty   as NE
import qualified Data.Set             as S
import qualified Data.Set.Internal    as S

data NESet a =
    NESet { nesV0  :: !a   -- ^ invariant: must be smaller than smallest value in set
          , nesSet :: !(Set a)
          }
  deriving (Typeable)

instance Eq a => Eq (NESet a) where
  t1 == t2  = (size t1 == size t2) && (toList t1 == toList t2)

instance Ord a => Ord (NESet a) where
    compare s1 s2 = compare (toList s1) (toList s2)

instance Show a => Show (NESet a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)

instance (Read a, Ord a) => Read (NESet a) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
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
  dataCast1      = gcast1

fromListConstr :: Constr
fromListConstr = mkConstr setDataType "fromList" [] Prefix

setDataType :: DataType
setDataType = mkDataType "Data.Set.Internal.Set" [fromListConstr]





nonEmptySet :: Set a -> Maybe (NESet a)
nonEmptySet = (fmap . uncurry) NESet . S.minView
{-# INLINE nonEmptySet #-}

toSet :: NESet a -> Set a
toSet (NESet x s) = insertMinSet x s
{-# INLINE toSet #-}

-- | /O(1)/. Create a singleton set.
singleton :: a -> NESet a
singleton x = NESet x S.empty
{-# INLINE singleton #-}

-- | /O(n*log n)/. Create a set from a list of elements.
-- If the list contains the same value twice, the last value is retained.

-- TODO: write manually and optimize to be equivalent to
-- 'fromDistinctAscList' if items are ordered, just like the actual
-- 'S.fromList'.
fromList :: Ord a => NonEmpty a -> NESet a
fromList (x :| s) = maybe (singleton x) (<> singleton x)
                  . nonEmptySet
                  $ S.fromList s
{-# INLINE fromList #-}

toList :: NESet a -> NonEmpty a
toList (NESet x s) = x :| S.toList s
{-# INLINE toList #-}

size :: NESet a -> Int
size (NESet _ s) = 1 + S.size s
{-# INLINE size #-}

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

-- | Left-biased union
instance Ord a => Semigroup (NESet a) where
    (<>) = union
    {-# INLINE (<>) #-}





-- | /O(n)/. Test if the internal set structure is valid.
valid :: Ord a => NESet a -> Bool
valid (NESet x s) = S.valid s
                  && all ((x <) . fst) (S.minView s)








insertMinSet :: a -> Set a -> Set a
insertMinSet x0 = go x0 x0
  where
    go :: a -> a -> Set a -> Set a
    go orig !_ Tip = S.singleton (lazy orig)
    go orig !x t@(Bin _ y l r)
        | l' `ptrEq` l = t
        | otherwise    = balanceL y l' r
      where
        !l' = go orig x l
{-# INLINABLE insertMinSet #-}

insertMaxSet :: a -> Set a -> Set a
insertMaxSet x0 = go x0 x0
  where
    go :: a -> a -> Set a -> Set a
    go orig !_ Tip = S.singleton (lazy orig)
    go orig !x t@(Bin _ y l r)
        | r' `ptrEq` r = t
        | otherwise    = balanceR y l r'
      where
        !r' = go orig x r
{-# INLINABLE insertMaxSet #-}

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

delta,ratio :: Int
delta = 3
ratio = 2







lazy :: a -> a
lazy x = x

ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y)
{-# INLINE ptrEq #-}
