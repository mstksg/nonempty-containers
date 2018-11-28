{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Sequence.NonEmpty (
  -- * Finite sequences
    NESeq ((:<||), (:||>))
  -- ** Conversions between empty and non-empty sequences
  , pattern IsNonEmpty
  , pattern IsEmpty
  , nonEmptySeq
  , toSeq
  , withNonEmpty
  , unsafeFromSeq
  , insertSeqAt
  -- * Construction
  , singleton
  , (<|)
  , (|>)
  , (><)
  , (|><)
  , (><|)
  , fromList
  -- ** Repetition
  , replicate
  , replicateA
  , replicateA1
  , replicateM
  , cycleTaking
  -- ** Iterative construction
  , iterateN
  , unfoldr
  , unfoldl
  -- * Deconstruction
  -- | Additional functions for deconstructing sequences are available
  -- via the 'Foldable' instance of 'Seq'.

  -- ** Queries
  , length

  -- * Scans
  , scanl
  , scanl1
  , scanr
  , scanr1
  -- * Sublists
  , tails
  , inits
  , chunksOf
  -- ** Sequential searches
  , takeWhileL
  , takeWhileR
  , dropWhileL
  , dropWhileR
  , spanl
  , spanr
  , breakl
  , breakr
  , partition
  , filter
  -- * Sorting
  , sort
  , sortBy
  , sortOn
  , unstableSort
  , unstableSortBy
  , unstableSortOn
  -- * Indexing
  , lookup
  , (!?)
  , index
  , adjust
  , adjust'
  , update
  , take
  , drop
  , insertAt
  , deleteAt
  , splitAt
  -- ** Indexing with predicates
  -- | These functions perform sequential searches from the left
  -- or right ends of the sequence  returning indices of matching
  -- elements.
  , elemIndexL
  , elemIndicesL
  , elemIndexR
  , elemIndicesR
  , findIndexL
  , findIndicesL
  , findIndexR
  , findIndicesR
  -- * Folds
  -- | General folds are available via the 'Foldable' instance of 'Seq'.
  , foldMapWithIndex
  , foldlWithIndex      -- TODO: foldlWithIndex1
  , foldrWithIndex
  -- * Transformations
  , mapWithIndex
  , traverseWithIndex
  , traverseWithIndex1
  , reverse
  , intersperse
  -- ** Zips and unzip
  , zip
  , zipWith
  , zip3
  , zipWith3
  , zip4
  , zipWith4
  , unzip
  , unzipWith
  ) where

import           Control.Applicative
import           Control.Monad hiding (replicateM)
import           Data.Bifunctor
import           Data.Foldable hiding (length)
import           Data.Functor.Apply
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Semigroup
import           Data.Sequence        (Seq(..))
import           Data.These
import           Prelude hiding       (length, scanl, scanl1, scanr, scanr1, splitAt, zip, zipWith, zip3, zipWith3, unzip, replicate, filter, reverse, lookup, take, drop)
import qualified Data.Sequence        as Seq

data NESeq a = NESeq { nesHead :: a
                     , nesTail :: !(Seq a)
                     }
  deriving Show

unsnoc :: NESeq a -> (Seq a, a)
unsnoc (x :<|| (xs :|> y)) = (x :<| xs, y)
unsnoc (x :<|| Empty     ) = (Empty   , x)
{-# INLINE unsnoc #-}

pattern (:<||) :: a -> Seq a -> NESeq a
pattern x :<|| xs = NESeq x xs
{-# COMPLETE (:<||) #-}

pattern (:||>) :: Seq a -> a -> NESeq a
pattern xs :||> x <- (unsnoc->(!xs, x))
  where
    (x :<| xs) :||> y = x :<|| (xs :|> y)
    Empty      :||> y = y :<|| Empty
{-# COMPLETE (:||>) #-}

infixr 5 :<||
infixr 5 :||>

-- | /O(1)/. The 'IsNonEmpty' and 'IsEmpty' patterns allow you to treat
-- a 'Seq' as if it were either a @'IsNonEmpty' n@ (where @n@ is a 'NESeq')
-- or an 'IsEmpty'.
--
-- For example, you can pattern match on a 'Seq':
--
-- @
-- safeHead :: 'Seq' Int -> Int
-- safeHead ('IsNonEmpty' (x :<|| _))  = x  -- here, user provided a non-empty sequence, and @n@ is the 'NESeq'
-- safeHead 'IsEmpty'                  = 0  -- here the user provided an empty sequence
-- @
--
-- Matching on @'IsNonEmpty' n@ means that the original 'Seq' was /not/
-- empty, and you have a verified-non-empty 'NESeq' @n@ to use.
--
-- A case statement handling both 'IsNonEmpty' and 'IsEmpty' provides
-- complete coverage.
--
-- This is a bidirectional pattern, so you can use 'IsNonEmpty' to convert
-- a 'NESeq' back into a 'Seq', obscuring its non-emptiness (see 'toSeq').
pattern IsNonEmpty :: NESeq a -> Seq a
pattern IsNonEmpty n <- (nonEmptySeq->Just n)
  where
    IsNonEmpty n = toSeq n

-- | /O(1)/. The 'IsNonEmpty' and 'IsEmpty' patterns allow you to treat
-- a 'Seq' as if it were either a @'IsNonEmpty' n@ (where @n@ is
-- a 'NESeq') or an 'IsEmpty'.
--
-- Matching on 'IsEmpty' means that the original 'Seq' was empty.
--
-- A case statement handling both 'IsNonEmpty' and 'IsEmpty' provides
-- complete coverage.
--
-- This is a bidirectional pattern, so you can use 'IsEmpty' as an
-- expression, and it will be interpreted as 'Data.Seq.empty'.
--
-- See 'IsNonEmpty' for more information.
pattern IsEmpty :: Seq a
pattern IsEmpty <- (Seq.null->True)
  where
    IsEmpty = Seq.empty

{-# COMPLETE IsNonEmpty, IsEmpty #-}

nonEmptySeq :: Seq a -> Maybe (NESeq a)
nonEmptySeq (x :<| xs) = Just $ x :<|| xs
nonEmptySeq Empty      = Nothing
{-# INLINE nonEmptySeq #-}

toSeq :: NESeq a -> Seq a
toSeq (x :<|| xs) = x :<| xs
{-# INLINE toSeq #-}

withNonEmpty :: r -> (NESeq a -> r) -> Seq a -> r
withNonEmpty def f = \case
    x :<| xs -> f (x :<|| xs)
    Empty    -> def
{-# INLINE withNonEmpty #-}

unsafeFromSeq :: Seq a -> NESeq a
unsafeFromSeq (x :<| xs) = x :<|| xs
unsafeFromSeq Empty      = errorWithoutStackTrace "NESeq.unsafeFromSeq: empty seq"
{-# INLINE unsafeFromSeq #-}

insertSeqAt :: Int -> a -> Seq a -> NESeq a
insertSeqAt i y
    | i <= 0    = (y :<||)
    | otherwise = \case
        x :<| xs -> x :<|| Seq.insertAt (i - 1) y xs
        Empty    -> y :<|| Seq.empty
{-# INLINE insertSeqAt #-}


singleton :: a -> NESeq a
singleton = (:<|| Seq.empty)
{-# INLINE singleton #-}

(<|) :: a -> NESeq a -> NESeq a
x <| xs = x :<|| toSeq xs
{-# INLINE (<|) #-}

(|>) :: NESeq a -> a -> NESeq a
(x :<|| xs) |> y = x :<|| (xs Seq.|> y)
{-# INLINE (|>) #-}

(><) :: NESeq a -> NESeq a -> NESeq a
(x :<|| xs) >< ys = x :<|| (xs Seq.>< toSeq ys)
{-# INLINE (><) #-}

(|><) :: NESeq a -> Seq a -> NESeq a
(x :<|| xs) |>< ys = x :<|| (xs Seq.>< ys)
{-# INLINE (|><) #-}

(><|) :: Seq a -> NESeq a -> NESeq a
xs ><| ys = withNonEmpty ys (>< ys) xs
{-# INLINE (><|) #-}

infixr 5 <|
infixl 5 |>
infixr 5 ><
infixr 5 |><
infixr 5 ><|

fromList :: NonEmpty a -> NESeq a
fromList (x :| xs) = x :<|| Seq.fromList xs
{-# INLINE fromList #-}

-- TODO: should this just return a Maybe (NESeq a)?  if so, then what's the
-- point?
replicate :: Int -> a -> NESeq a
replicate n x
    | n < 1     = error "NESeq.replicate: must take a positive integer argument"
    | otherwise = x :<|| Seq.replicate (n - 1) x
{-# INLINE replicate #-}

replicateA :: Applicative f => Int -> f a -> f (NESeq a)
replicateA n x
    | n < 1     = error "NESeq.replicateA: must take a positive integer argument"
    | otherwise = liftA2 (:<||) x (Seq.replicateA (n - 1) x)
{-# INLINE replicateA #-}

replicateA1 :: Apply f => Int -> f a -> f (NESeq a)
replicateA1 n x
    | n < 1     = error "NESeq.replicateA1: must take a positive integer argument"
    | otherwise = case runMaybeApply (Seq.replicateA (n - 1) (MaybeApply (Left x))) of
        Left  xs -> (:<||)    <$> x <.> xs
        Right xs -> (:<|| xs) <$> x
{-# INLINE replicateA1 #-}

replicateM :: Applicative m => Int -> m a -> m (NESeq a)
replicateM = replicateA
{-# INLINE replicateM #-}

cycleTaking :: Int -> NESeq a -> NESeq a
cycleTaking n xs0@(x :<|| xs)
    | n < 1             = error "NESeq.cycleTaking: must take a positive integer argument"
    | n < Seq.length xs = x :<|| Seq.take (n - 1) xs
    | otherwise         = xs0 |>< Seq.cycleTaking (n - length xs0) (toSeq xs0)
{-# INLINE cycleTaking #-}

iterateN :: Int -> (a -> a) -> a -> NESeq a
iterateN n f x = x :<|| Seq.iterateN (n - 1) f (f x)
{-# INLINE iterateN #-}

unfoldr :: (a -> (b, Maybe a)) -> a -> NESeq b
unfoldr f = go
  where
    go x0 = y :<|| maybe Seq.empty (toSeq . go) x1
      where
        (y, x1) = f x0
{-# INLINE unfoldr #-}

unfoldl :: (a -> (b, Maybe a)) -> a -> NESeq b
unfoldl f = go
  where
    go x0 = maybe Seq.empty (toSeq . go) x1 :||> y
      where
        (y, x1) = f x0
{-# INLINE unfoldl #-}

length :: NESeq a -> Int
length (_ :<|| xs) = 1 + Seq.length xs
{-# INLINE length #-}

scanl :: (a -> b -> a) -> a -> NESeq b -> NESeq a
scanl f y0 (x :<|| xs) = y0 :<|| Seq.scanl f (f y0 x) xs
{-# INLINE scanl #-}

scanl1 :: (a -> a -> a) -> NESeq a -> NESeq a
scanl1 f (x :<|| xs) = withNonEmpty (singleton x) (scanl f x) xs
{-# INLINE scanl1 #-}

scanr :: (a -> b -> b) -> b -> NESeq a -> NESeq b
scanr f y0 (xs :||> x) = Seq.scanr f (f x y0) xs :||> y0
{-# INLINE scanr #-}

scanr1 :: (a -> a -> a) -> NESeq a -> NESeq a
scanr1 f (xs :||> x) = withNonEmpty (singleton x) (scanr f x) xs
{-# INLINE scanr1 #-}

tails :: NESeq a -> NESeq (NESeq a)
tails xs@(_ :<|| ys) = withNonEmpty (singleton xs) ((xs <|) . tails) ys
{-# INLINABLE tails #-}

inits :: NESeq a -> NESeq (NESeq a)
inits xs@(ys :||> _) = withNonEmpty (singleton xs) ((|> xs) . inits) ys
{-# INLINABLE inits #-}

chunksOf :: Int -> NESeq a -> NESeq (NESeq a)
chunksOf n = go
  where
    go xs = case splitAt n xs of
      This  ys    -> singleton ys
      That     _  -> e
      These ys zs -> ys <| go zs
    e = error "chunksOf: A non-empty sequence can only be broken up into positively-sized chunks."
{-# INLINABLE chunksOf #-}

takeWhileL :: (a -> Bool) -> NESeq a -> Seq a
takeWhileL p (x :<|| xs)
    | p x       = x Seq.<| Seq.takeWhileL p xs
    | otherwise = Seq.empty
{-# INLINE takeWhileL #-}

takeWhileR :: (a -> Bool) -> NESeq a -> Seq a
takeWhileR p (xs :||> x)
    | p x       = Seq.takeWhileR p xs Seq.|> x
    | otherwise = Seq.empty
{-# INLINE takeWhileR #-}

dropWhileL :: (a -> Bool) -> NESeq a -> Seq a
dropWhileL p xs0@(x :<|| xs)
    | p x       = Seq.dropWhileL p xs
    | otherwise = toSeq xs0
{-# INLINE dropWhileL #-}

dropWhileR :: (a -> Bool) -> NESeq a -> Seq a
dropWhileR p xs0@(xs :||> x)
    | p x       = Seq.dropWhileR p xs
    | otherwise = toSeq xs0
{-# INLINE dropWhileR #-}

spanl :: (a -> Bool) -> NESeq a -> These (NESeq a) (NESeq a)
spanl p xs0@(x :<|| xs)
    | p x       = case (nonEmptySeq ys, nonEmptySeq zs) of
        (Nothing , Nothing ) -> This  (singleton x)
        (Just _  , Nothing ) -> This  xs0
        (Nothing , Just zs') -> These (singleton x) zs'
        (Just ys', Just zs') -> These (x <| ys')    zs'
    | otherwise = That xs0
  where
    (ys, zs) = Seq.spanl p xs
{-# INLINABLE spanl #-}

spanr :: (a -> Bool) -> NESeq a -> These (NESeq a) (NESeq a)
spanr p xs0@(xs :||> x)
    | p x       = case (nonEmptySeq ys, nonEmptySeq zs) of
        (Nothing , Nothing ) -> That  (singleton x)
        (Just ys', Nothing ) -> These ys'           (singleton x)
        (Nothing , Just _  ) -> That  xs0
        (Just ys', Just zs') -> These ys'           (zs' |> x)
    | otherwise = That xs0
  where
    (ys, zs) = Seq.spanr p xs
{-# INLINABLE spanr #-}

breakl :: (a -> Bool) -> NESeq a -> These (NESeq a) (NESeq a)
breakl p = spanl (not . p)
{-# INLINE breakl #-}

breakr :: (a -> Bool) -> NESeq a -> These (NESeq a) (NESeq a)
breakr p = spanr (not . p)
{-# INLINE breakr #-}

partition :: (a -> Bool) -> NESeq a -> These (NESeq a) (NESeq a)
partition p xs0@(x :<|| xs) = case (nonEmptySeq ys, nonEmptySeq zs) of
    (Nothing , Nothing )
      | p x       -> This  (singleton x)
      | otherwise -> That                (singleton x)
    (Just ys', Nothing )
      | p x       -> This  xs0
      | otherwise -> These ys'           (singleton x)
    (Nothing, Just zs' )
      | p x       -> These (singleton x) zs'
      | otherwise -> That                xs0
    (Just ys', Just zs')
      | p x       -> These (x <| ys')    zs'
      | otherwise -> These ys'           (x <| zs')
  where
    (ys, zs) = Seq.partition p xs
{-# INLINABLE partition #-}

filter :: (a -> Bool) -> NESeq a -> Seq a
filter p (x :<|| xs)
    | p x       = x Seq.<| Seq.filter p xs
    | otherwise = Seq.filter p xs
{-# INLINE filter #-}

sort :: Ord a => NESeq a -> NESeq a
sort = sortBy compare
{-# INLINE sort #-}

sortBy :: (a -> a -> Ordering) -> NESeq a -> NESeq a
sortBy c (x :<|| xs) = withNonEmpty (singleton x) (insertBy c x)
                     . Seq.sortBy c
                     $ xs
{-# INLINE sortBy #-}

sortOn :: Ord b => (a -> b) -> NESeq a -> NESeq a
sortOn f (x :<|| xs) = withNonEmpty (singleton x) (insertOn f x)
                     . Seq.sortOn f
                     $ xs
{-# INLINE sortOn #-}

unstableSort :: Ord a => NESeq a -> NESeq a
unstableSort = unstableSortBy compare
{-# INLINE unstableSort #-}

unstableSortBy :: (a -> a -> Ordering) -> NESeq a -> NESeq a
unstableSortBy c (x :<|| xs) = withNonEmpty (singleton x) (insertBy c x)
                     . Seq.unstableSortBy c
                     $ xs
{-# INLINE unstableSortBy #-}

unstableSortOn :: Ord b => (a -> b) -> NESeq a -> NESeq a
unstableSortOn f (x :<|| xs) = withNonEmpty (singleton x) (insertOn f x)
                     . Seq.unstableSortOn f
                     $ xs
{-# INLINE unstableSortOn #-}

insertBy :: (a -> a -> Ordering) -> a -> NESeq a -> NESeq a
insertBy c x xs = case spanl ltx xs of
    This  ys    -> ys |> x
    That     zs -> x <| zs
    These ys zs -> ys >< (x <| zs)
  where
    ltx y = c x y == LT
{-# INLINABLE insertBy #-}

insertOn :: Ord b => (a -> b) -> a -> NESeq a -> NESeq a
insertOn f x xs = case spanl ltx xs of
    This  ys    -> ys |> x
    That     zs -> x <| zs
    These ys zs -> ys >< (x <| zs)
  where
    fx = f x
    ltx y = fx < f y
{-# INLINABLE insertOn #-}

lookup :: Int -> NESeq a -> Maybe a
lookup 0 (x :<|| _ ) = Just x
lookup i (_ :<|| xs) = Seq.lookup (i - 1) xs
{-# INLINE lookup #-}

(!?) :: NESeq a -> Int -> Maybe a
(!?) = flip lookup
{-# INLINE (!?) #-}

index :: NESeq a -> Int -> a
index (x :<|| _ ) 0 = x
index (_ :<|| xs) i = xs `Seq.index` (i - 1)
{-# INLINE index #-}

adjust :: (a -> a) -> Int -> NESeq a -> NESeq a
adjust f 0 (x :<|| xs) = f x :<|| xs
adjust f i (x :<|| xs) = x :<|| Seq.adjust f (i - 1) xs
{-# INLINE adjust #-}

adjust' :: (a -> a) -> Int -> NESeq a -> NESeq a
adjust' f 0 (x :<|| xs) = let !y  = f x                     in y :<|| xs
adjust' f i (x :<|| xs) = let !ys = Seq.adjust f (i - 1) xs in x :<|| ys
{-# INLINE adjust' #-}

update :: Int -> a -> NESeq a -> NESeq a
update 0 y (_ :<|| xs) = y :<|| xs
update i y (x :<|| xs) = x :<|| Seq.update (i - 1) y xs
{-# INLINE update #-}

take :: Int -> NESeq a -> Seq a
take i (x :<|| xs)
    | i <= 0    = Seq.empty
    | otherwise = x Seq.<| Seq.take (i - 1) xs
{-# INLINE take #-}

drop :: Int -> NESeq a -> Seq a
drop i xs0@(_ :<|| xs)
    | i <= 0    = toSeq xs0
    | otherwise = Seq.drop (i - 1) xs
{-# INLINE drop #-}

insertAt :: Int -> a -> NESeq a -> NESeq a
insertAt i y xs0@(x :<|| xs)
    | i <= 0    = y <| xs0
    | otherwise = x :<|| Seq.insertAt (i - 1) y xs
{-# INLINE insertAt #-}

deleteAt :: Int -> NESeq a -> Seq a
deleteAt i (x :<|| xs)
    | i <= 0    = xs
    | otherwise = x Seq.<| Seq.deleteAt (i - 1) xs
{-# INLINE deleteAt #-}

splitAt :: Int -> NESeq a -> These (NESeq a) (NESeq a)
splitAt 0 xs0             = That xs0
splitAt n xs0@(x :<|| xs) = case (nonEmptySeq ys, nonEmptySeq zs) of
    (Nothing , Nothing ) -> This  (singleton x)
    (Just _  , Nothing ) -> This  xs0
    (Nothing , Just zs') -> These (singleton x) zs'
    (Just ys', Just zs') -> These (x <| ys')    zs'
  where
    (ys, zs) = Seq.splitAt (n - 1) xs
{-# INLINABLE splitAt #-}

elemIndexL :: Eq a => a -> NESeq a -> Maybe Int
elemIndexL x = findIndexL (== x)
{-# INLINE elemIndexL #-}

elemIndexR :: Eq a => a -> NESeq a -> Maybe Int
elemIndexR x = findIndexR (== x)
{-# INLINE elemIndexR #-}

elemIndicesL :: Eq a => a -> NESeq a -> [Int]
elemIndicesL x = findIndicesL (== x)
{-# INLINE elemIndicesL #-}

elemIndicesR :: Eq a => a -> NESeq a -> [Int]
elemIndicesR x = findIndicesR (== x)
{-# INLINE elemIndicesR #-}

findIndexL :: (a -> Bool) -> NESeq a -> Maybe Int
findIndexL p (x :<|| xs) = here_ <|> there_
  where
    here_  = 0 <$ guard (p x)
    there_ = (+ 1) <$> Seq.findIndexL p xs
{-# INLINE findIndexL #-}

findIndexR :: (a -> Bool) -> NESeq a -> Maybe Int
findIndexR p (xs :||> x) = here_ <|> there_
  where
    here_  = Seq.length xs <$ guard (p x)
    there_ = Seq.findIndexL p xs
{-# INLINE findIndexR #-}

findIndicesL :: (a -> Bool) -> NESeq a -> [Int]
findIndicesL p (x :<|| xs)
    | p x       = 0 : ixs
    | otherwise = ixs
  where
    ixs = (+ 1) <$> Seq.findIndicesL p xs
{-# INLINE findIndicesL #-}

findIndicesR :: (a -> Bool) -> NESeq a -> [Int]
findIndicesR p (xs :||> x)
    | p x       = Seq.length xs : ixs
    | otherwise = ixs
  where
    ixs = Seq.findIndicesL p xs
{-# INLINE findIndicesR #-}

foldMapWithIndex :: Semigroup m => (Int -> a -> m) -> NESeq a -> m
foldMapWithIndex f (x :<|| xs) = maybe (f 0 x) (f 0 x <>)
                               . getOption
                               . Seq.foldMapWithIndex (\i -> Option . Just . f (i + 1))
                               $ xs
{-# INLINE foldMapWithIndex #-}

foldlWithIndex :: (b -> Int -> a -> b) -> b -> NESeq a -> b
foldlWithIndex f z (x :<|| xs) =
    Seq.foldlWithIndex (\y -> f y . (+ 1)) (f z 0 x) xs
{-# INLINE foldlWithIndex #-}

foldrWithIndex :: (Int -> a -> b -> b) -> b -> NESeq a -> b
foldrWithIndex f z (x :<|| xs) = f 0 x . Seq.foldrWithIndex (f . (+ 1)) z $ xs
{-# INLINE foldrWithIndex #-}

mapWithIndex :: (Int -> a -> b) -> NESeq a -> NESeq b
mapWithIndex f (x :<|| xs) = f 0 x :<|| Seq.mapWithIndex (f . (+ 1)) xs
{-# INLINE mapWithIndex #-}

traverseWithIndex :: Applicative f => (Int -> a -> f b) -> NESeq a -> f (NESeq b)
traverseWithIndex f (x :<|| xs) = (:<||) <$> f 0 x <*> Seq.traverseWithIndex (f . (+ 1)) xs
{-# INLINE traverseWithIndex #-}

traverseWithIndex1 :: Apply f => (Int -> a -> f b) -> NESeq a -> f (NESeq b)
traverseWithIndex1 f (x :<|| xs) = case runMaybeApply xs' of
    Left  ys -> (:<||)    <$> f 0 x <.> ys
    Right ys -> (:<|| ys) <$> f 0 x
  where
    xs' = Seq.traverseWithIndex (\i -> MaybeApply . Left . f (i+1)) xs
{-# INLINE traverseWithIndex1 #-}

reverse :: NESeq a -> NESeq a
reverse (x :<|| xs) = Seq.reverse xs :||> x
{-# INLINE reverse #-}

intersperse :: a -> NESeq a -> NESeq a
intersperse z (x :<|| xs) = x :<|| (z Seq.<| Seq.intersperse z xs)
{-# INLINE intersperse #-}

zip :: NESeq a -> NESeq b -> NESeq (a, b)
zip (x :<|| xs) (y :<|| ys) = (x, y) :<|| Seq.zip xs ys
{-# INLINE zip #-}

zipWith :: (a -> b -> c) -> NESeq a -> NESeq b -> NESeq c
zipWith f (x :<|| xs) (y :<|| ys) = f x y :<|| Seq.zipWith f xs ys
{-# INLINE zipWith #-}

zip3 :: NESeq a -> NESeq b -> NESeq c -> NESeq (a, b, c)
zip3 (x :<|| xs) (y :<|| ys) (z :<|| zs) = (x, y, z) :<|| Seq.zip3 xs ys zs
{-# INLINE zip3 #-}

zipWith3 :: (a -> b -> c -> d) -> NESeq a -> NESeq b -> NESeq c -> NESeq d
zipWith3 f (x :<|| xs) (y :<|| ys) (z :<|| zs) = f x y z :<|| Seq.zipWith3 f xs ys zs
{-# INLINE zipWith3 #-}

zip4 :: NESeq a -> NESeq b -> NESeq c -> NESeq d -> NESeq (a, b, c, d)
zip4 (x :<|| xs) (y :<|| ys) (z :<|| zs) (r :<|| rs) = (x, y, z, r) :<|| Seq.zip4 xs ys zs rs
{-# INLINE zip4 #-}

zipWith4 :: (a -> b -> c -> d -> e) -> NESeq a -> NESeq b -> NESeq c -> NESeq d -> NESeq e
zipWith4 f (x :<|| xs) (y :<|| ys) (z :<|| zs) (r :<|| rs) = f x y z r :<|| Seq.zipWith4 f xs ys zs rs
{-# INLINE zipWith4 #-}

unzip :: NESeq (a, b) -> (NESeq a, NESeq b)
unzip ((x, y) :<|| xys) = bimap (x :<||) (y :<||) . Seq.unzip $ xys
{-# INLINE unzip #-}

unzipWith :: (a -> (b, c)) -> NESeq a -> (NESeq b, NESeq c)
unzipWith f (x :<|| xs) = bimap (y :<||) (z :<||) . Seq.unzipWith f $ xs
  where
    (y, z) = f x
{-# INLINE unzipWith #-}
