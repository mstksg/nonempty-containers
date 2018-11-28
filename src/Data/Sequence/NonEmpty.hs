{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Sequence.NonEmpty (
    -- * Finite sequences
    NESeq ((:<||), (:||>))
    -- ** Conversions between empty and non-empty sequences
  -- , pattern IsNonEmpty
  -- , pattern IsEmpty
  , nonEmptySeq
  , toSeq
  , withNonEmpty
  , unsafeFromSeq
    -- * Construction
  , singleton
  , (<|)
  , (|>)
  , (><)
  , fromList
  --   -- ** Repetition
  -- , replicate
  -- , replicateA
  -- , replicateM
  -- , cycleTaking
  --   -- ** Iterative construction
  , iterateN
  -- , unfoldr
  -- , unfoldl
  --   -- * Deconstruction
  --   -- | Additional functions for deconstructing sequences are available
  --   -- via the 'Foldable' instance of 'Seq'.

    -- ** Queries
  , length
  --   -- ** Views
  -- , ViewL(..)
  -- , viewl
  -- , ViewR(..)
  -- , viewr
  --   -- * Scans
  -- , scanl
  -- , scanl1
  -- , scanr
  -- , scanr1
  --   -- * Sublists
  -- , tails
  -- , inits
  -- , chunksOf
  --   -- ** Sequential searches
  -- , takeWhileL
  -- , takeWhileR
  -- , dropWhileL
  -- , dropWhileR
  -- , spanl
  -- , spanr
  -- , breakl
  -- , breakr
  -- , partition
  -- , filter
  --   -- * Sorting
  -- , sort
  -- , sortBy
  -- , sortOn
  -- , unstableSort
  -- , unstableSortBy
  -- , unstableSortOn
  --   -- * Indexing
  -- , lookup
  -- , (!?)
  -- , index
  -- , adjust
  -- , adjust'
  -- , update
  -- , take
  -- , drop
  -- , insertAt
  -- , deleteAt
  -- , splitAt
  --   -- ** Indexing with predicates
  --   -- | These functions perform sequential searches from the left
  --   -- or right ends of the sequence  returning indices of matching
  --   -- elements.
  -- , elemIndexL
  -- , elemIndicesL
  -- , elemIndexR
  -- , elemIndicesR
  -- , findIndexL
  -- , findIndicesL
  -- , findIndexR
  -- , findIndicesR
  --   -- * Folds
  --   -- | General folds are available via the 'Foldable' instance of 'Seq'.
  -- , foldMapWithIndex
  -- , foldlWithIndex
  -- , foldrWithIndex
  --   -- * Transformations
  -- , mapWithIndex
  -- , traverseWithIndex
  -- , reverse
  -- , intersperse
  --   -- ** Zips and unzip
  -- , zip
  -- , zipWith
  -- , zip3
  -- , zipWith3
  -- , zip4
  -- , zipWith4
  -- , unzip
  -- , unzipWith
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Sequence      (Seq(..))
import           Prelude hiding     (length)
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence      as Seq

data NESeq a = a :<|| Seq a

unsnoc :: NESeq a -> (Seq a, a)
unsnoc (x :<|| (xs :|> y)) = (x :<| xs, y)
unsnoc (x :<|| Empty     ) = (Empty   , x)
{-# INLINE unsnoc #-}

pattern (:||>) :: Seq a -> a -> NESeq a
pattern xs :||> x <- (unsnoc->(xs, x))
  where
    (x :<| xs) :||> y = x :<|| (xs :|> y)
    Empty      :||> y = y :<|| Empty
{-# COMPLETE (:<||), (:||>) #-}

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

singleton :: a -> NESeq a
singleton = (:<|| Seq.empty)

(<|) :: a -> NESeq a -> NESeq a
x <| xs = x :<|| toSeq xs

(|>) :: NESeq a -> a -> NESeq a
(x :<|| xs) |> y = x :<|| (xs Seq.|> y)

(><) :: NESeq a -> NESeq a -> NESeq a
(x :<|| xs) >< ys = x :<|| (xs Seq.>< toSeq ys)

fromList :: NonEmpty a -> NESeq a
fromList (x :| xs) = x :<|| Seq.fromList xs

iterateN :: Int -> (a -> a) -> a -> NESeq a
iterateN n f x = x :<|| Seq.iterateN (n - 1) f (f x)
    
length :: NESeq a -> Int
length (_ :<|| xs) = 1 + Seq.length xs
