{-# LANGUAGE BangPatterns #-}

module Data.Set.NonEmpty (
  -- * Non-Empty Set Type
    NESet
  -- ** Conversions between empty and non-empty maps
  -- , pattern IsNonEmpty
  -- , pattern IsEmpty
  , nonEmptySet
  , toSet
  , withNESet
  , insertSet
  , insertSetMin
  , insertSetMax

  -- * Construction
  , singleton
  , fromList
  , fromAscList
  , fromDescList
  , fromDistinctAscList
  , fromDistinctDescList
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
  , elems
  , toList
  , toAscList
  , toDescList

  -- * Debugging
  , valid
  ) where

import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Set                   (Set)
import           Data.Set.NonEmpty.Internal
import           Prelude hiding             (foldr, foldl)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as S

withNESet
    :: r                  -- ^ value to return if set is empty
    -> (NESet a -> r)     -- ^ function to apply if set is not empty
    -> Set a
    -> r
withNESet def f = maybe def f . nonEmptySet
{-# INLINE withNESet #-}

insertSet :: Ord a => a -> Set a -> NESet a
insertSet x = withNESet (singleton x) (insert x)

insertSetMin :: a -> Set a -> NESet a
insertSetMin = NESet
{-# INLINE insertSetMin #-}

insertSetMax :: a -> Set a -> NESet a
insertSetMax x = withNESet (singleton x) go
  where
    go (NESet x0 s0) = NESet x0 . insertMaxSet x $ s0
{-# INLINE insertSetMax #-}

fromAscList :: Eq a => NonEmpty a -> NESet a
fromAscList = fromDistinctAscList . combineEq
{-# INLINE fromAscList #-}

fromDistinctAscList :: NonEmpty a -> NESet a
fromDistinctAscList (x :| xs) = insertSetMin x
                              . S.fromDistinctAscList
                              $ xs
{-# INLINE fromDistinctAscList #-}

fromDescList :: Eq a => NonEmpty a -> NESet a
fromDescList = fromDistinctDescList . combineEq
{-# INLINE fromDescList #-}

fromDistinctDescList :: NonEmpty a -> NESet a
fromDistinctDescList (x :| xs) = insertSetMax x
                               . S.fromDistinctDescList
                               $ xs
{-# INLINE fromDistinctDescList #-}


foldr1' :: (a -> a -> a) -> NESet a -> a
foldr1' f (NESet x s) = case S.maxView s of
    Nothing      -> x
    Just (y, s') -> let !z = S.foldr' f y s' in x `f` z
{-# INLINE foldr1' #-}

foldl1' :: (a -> a -> a) -> NESet a -> a
foldl1' f (NESet x s) = S.foldl' f x s
{-# INLINE foldl1' #-}

elems :: NESet a -> NonEmpty a
elems = toList
{-# INLINE elems #-}

toAscList :: NESet a -> NonEmpty a
toAscList = toList
{-# INLINE toAscList #-}

toDescList :: NESet a -> NonEmpty a
toDescList (NESet x s) = maybe x0 (<> x0)
                       . NE.nonEmpty
                       . S.toList
                       $ s
  where
    x0 = x :| []
{-# INLINE toDescList #-}

-- ---------------------------
-- Combining functions
-- ---------------------------
--
-- Code comes from "Data.Set.Internal" from containers, modified slightly
-- to work with NonEmpty
--
-- Copyright   :  (c) Daan Leijen 2002

combineEq :: Eq a => NonEmpty a -> NonEmpty a
combineEq (x :| xs) = go x xs
  where
    go z [] = z :| []
    go z (y:ys)
      | z == y    = go z ys
      | otherwise = z NE.<| go y ys
