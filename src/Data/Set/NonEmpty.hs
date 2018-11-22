{-# LANGUAGE BangPatterns #-}

module Data.Set.NonEmpty (
  -- * Set type
    NESet
  -- * Construction
  , singleton
  , fromList
  -- , fromAscList
  -- , fromDescList
  -- , fromDistinctAscList
  -- , fromDistinctDescList
  -- , powerSet

  -- -- * Insertion
  , insert

  -- -- * Deletion
  -- , delete

  -- -- * Query
  -- , member
  -- , notMember
  -- , lookupLT
  -- , lookupGT
  -- , lookupLE
  -- , lookupGE
  , size
  -- , isSubsetOf
  -- , isProperSubsetOf
  -- , disjoint

  -- -- * Combine
  , union
  , unions
  -- , difference
  -- , (\\)
  -- , intersection
  -- , cartesianProduct
  -- , disjointUnion

  -- -- * Filter
  -- , filter
  -- , takeWhileAntitone
  -- , dropWhileAntitone
  -- , spanAntitone
  -- , partition
  -- , split
  -- , splitMember
  -- , splitRoot

  -- -- * Indexed
  -- , lookupIndex
  -- , findIndex
  -- , elemAt
  -- , deleteAt
  -- , take
  -- , drop
  -- , splitAt

  -- -- * Map
  -- , map
  -- , mapMonotonic

  -- * Folds
  , foldr
  , foldl
  , foldr1
  , foldl1
  -- ** Strict folds
  , foldr'
  , foldl'
  , foldr1'
  , foldl1'

  -- -- * Min\/Max
  -- , lookupMin
  -- , lookupMax
  -- , findMin
  -- , findMax
  -- , deleteMin
  -- , deleteMax
  -- , deleteFindMin
  -- , deleteFindMax
  -- , maxView
  -- , minView

  -- -- * Conversion

  -- -- ** List
  -- , elems
  , toList
  -- , toAscList
  -- , toDescList

  -- * Debugging
  , valid
  ) where

import           Data.Set.NonEmpty.Internal
import           Prelude hiding (foldr, foldl)
import qualified Data.Set                   as S

foldr1' :: (a -> a -> a) -> NESet a -> a
foldr1' f (NESet x s) = case S.maxView s of
    Nothing      -> x
    Just (y, s') -> let !z = S.foldr' f y s' in x `f` z
{-# INLINE foldr1' #-}

foldl1' :: (a -> a -> a) -> NESet a -> a
foldl1' f (NESet x s) = S.foldl' f x s
{-# INLINE foldl1' #-}
