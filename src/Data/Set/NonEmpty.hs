{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

module Data.Set.NonEmpty (
  -- * Non-Empty Set Type
    NESet
  -- ** Conversions between empty and non-empty maps
  , pattern IsNonEmpty
  , pattern IsEmpty
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
  , delete

  -- -- * Query
  , member
  , notMember
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE
  , size
  , isSubsetOf
  , isProperSubsetOf
  , disjoint

  -- -- * Combine
  , union
  , unions
  , difference
  , (\\)
  , intersection
  , cartesianProduct
  , disjointUnion

  -- -- * Filter
  , filter
  , takeWhileAntitone
  , dropWhileAntitone
  , spanAntitone
  , partition
  , split
  , splitMember
  , splitRoot

  -- -- * Indexed
  , lookupIndex
  , findIndex
  , elemAt
  , deleteAt
  , take
  , drop
  , splitAt

  -- -- * Map
  , map
  , mapMonotonic

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
  , findMin
  , findMax
  , deleteMin
  , deleteMax
  , deleteFindMin
  , deleteFindMax

  -- -- * Conversion

  -- -- ** List
  , elems
  , toList
  , toAscList
  , toDescList

  -- * Debugging
  , valid
  ) where

import           Control.Applicative
import           Data.Bifunctor
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Maybe
import           Data.Set                   (Set)
import           Data.Set.NonEmpty.Internal
import           Data.These
import           Prelude hiding             (foldr, foldl, filter, map, take, drop, splitAt)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as S

pattern IsNonEmpty :: NESet a -> Set a
pattern IsNonEmpty n <- (nonEmptySet->Just n)
  where
    IsNonEmpty n = toSet n

pattern IsEmpty :: Set a
pattern IsEmpty <- (S.null->True)
  where
    IsEmpty = S.empty

{-# COMPLETE IsNonEmpty, IsEmpty #-}

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

-- powerSet
--     :: NESet a
--     -> NESet (NESet a)
-- powerSet (NESet x s) = _ $ S.powerSet s

delete :: Ord a => a -> NESet a -> Set a
delete x n@(NESet x0 s) = case compare x x0 of
    LT -> toSet n
    EQ -> s
    GT -> insertMinSet x0 . S.delete x $ s
{-# INLINE delete #-}

member :: Ord a => a -> NESet a -> Bool
member x (NESet x0 s) = case compare x x0 of
    LT -> False
    EQ -> True
    GT -> S.member x s
{-# INLINE member #-}

notMember :: Ord a => a -> NESet a -> Bool
notMember k = not . member k
{-# INLINE notMember #-}

lookupLT :: Ord a => a -> NESet a -> Maybe a
lookupLT x (NESet x0 s) = case compare x x0 of
    LT -> Nothing
    EQ -> Nothing
    GT -> S.lookupLT x s <|> Just x0
{-# INLINE lookupLT #-}

lookupGT :: Ord a => a -> NESet a -> Maybe a
lookupGT x (NESet x0 s) = case compare x x0 of
    LT -> Just x0
    EQ -> S.lookupMin s
    GT -> S.lookupGT x s
{-# INLINE lookupGT #-}

lookupLE :: Ord a => a -> NESet a -> Maybe a
lookupLE x (NESet x0 s) = case compare x x0 of
    LT -> Nothing
    EQ -> Just x0
    GT -> S.lookupLE x s <|> Just x0
{-# INLINE lookupLE #-}

lookupGE :: Ord a => a -> NESet a -> Maybe a
lookupGE x (NESet x0 s) = case compare x x0 of
    LT -> Just x0
    EQ -> Just x0
    GT -> S.lookupGE x s
{-# INLINE lookupGE #-}

isSubsetOf
    :: Ord a
    => NESet a
    -> NESet a
    -> Bool
isSubsetOf (NESet x s0) (toSet->s1) = x `S.member` s1
                                   && s0 `S.isSubsetOf` s1
{-# INLINE isSubsetOf #-}

isProperSubsetOf
    :: Ord a
    => NESet a
    -> NESet a
    -> Bool
isProperSubsetOf s0 s1 = S.size (nesSet s0) < S.size (nesSet s1)
                      && s0 `isSubsetOf` s1
{-# INLINE isProperSubsetOf #-}

disjoint
    :: Ord a
    => NESet a
    -> NESet a
    -> Bool
disjoint n1@(NESet x1 s1) n2@(NESet x2 s2) = case compare x1 x2 of
    -- x1 is not in n2
    LT -> s1 `S.disjoint` toSet n2
    -- k1 and k2 are a part of the result
    EQ -> False
    -- k2 is not in n1
    GT -> toSet n1 `S.disjoint` s2
{-# INLINE disjoint #-}

difference
    :: Ord a
    => NESet a
    -> NESet a
    -> Set a
difference n1@(NESet x1 s1) n2@(NESet x2 s2) = case compare x1 x2 of
    -- x1 is not in n2, so cannot be deleted
    LT -> insertMinSet x1 $ s1 `S.difference` toSet n2
    -- x2 deletes x1, and only x1
    EQ -> s1 `S.difference` s2
    -- x2 is not in n1, so cannot delete anything, so we can just difference n1 // s2.
    GT -> toSet n1 `S.difference` s2
{-# INLINE difference #-}

-- | Same as 'difference'.
(\\)
    :: Ord a
    => NESet a
    -> NESet a
    -> Set a
(\\) = difference
{-# INLINE (\\) #-}

intersection
    :: Ord a
    => NESet a
    -> NESet a
    -> Set a
intersection n1@(NESet x1 s1) n2@(NESet x2 s2) = case compare x1 x2 of
    -- x1 is not in n2
    LT -> s1 `S.intersection` toSet n2
    -- x1 and x2 are a part of the result
    EQ -> insertMinSet x1 $ s1 `S.intersection` s2
    -- x2 is not in n1
    GT -> toSet n1 `S.intersection` s2
{-# INLINE intersection #-}

-- TODO: better way to do this? maybe using merge?
cartesianProduct
    :: NESet a
    -> NESet b
    -> NESet (a, b)
cartesianProduct n1@(NESet x1 _) n2@(NESet x2 _) = NESet (x1, x2) s3
  where
    s3 = S.deleteMin $ toSet n1 `S.cartesianProduct` toSet n2
{-# INLINE cartesianProduct #-}

disjointUnion
    :: NESet a
    -> NESet b
    -> NESet (Either a b)
disjointUnion (NESet x1 s1) n2 = NESet (Left x1)
                                       (s1 `S.disjointUnion` toSet n2)
{-# INLINE disjointUnion #-}

filter
    :: (a -> Bool)
    -> NESet a
    -> Set a
filter f (NESet x s1)
    | f x       = insertMinSet x . S.filter f $ s1
    | otherwise = S.filter f s1
{-# INLINE filter #-}

takeWhileAntitone
    :: (a -> Bool)
    -> NESet a
    -> Set a
takeWhileAntitone f (NESet x s)
    | f x       = insertMinSet x . S.takeWhileAntitone f $ s
    | otherwise = S.empty
{-# INLINE takeWhileAntitone #-}

dropWhileAntitone
    :: (a -> Bool)
    -> NESet a
    -> Set a
dropWhileAntitone f n@(NESet x s)
    | f x       = S.dropWhileAntitone f s
    | otherwise = toSet n
{-# INLINE dropWhileAntitone #-}

spanAntitone
    :: (a -> Bool)
    -> NESet a
    -> These (NESet a) (NESet a)
spanAntitone f n@(NESet x s0)
    | f x       = case (nonEmptySet s1, nonEmptySet s2) of
        (Nothing, Nothing) -> This  n
        (Just _ , Nothing) -> This  n
        (Nothing, Just n2) -> These (singleton x)       n2
        (Just _ , Just n2) -> These (insertSetMin x s1) n2
    | otherwise = That n
  where
    (s1, s2) = S.spanAntitone f s0
{-# INLINE spanAntitone #-}

partition
    :: (a -> Bool)
    -> NESet a
    -> These (NESet a) (NESet a)
partition f n@(NESet x s0) = case (nonEmptySet s1, nonEmptySet s2) of
    (Nothing, Nothing)
      | f x       -> This  n
      | otherwise -> That                      n
    (Just n1, Nothing)
      | f x       -> This  n
      | otherwise -> These n1                  (singleton x)
    (Nothing, Just n2)
      | f x       -> These (singleton x)       n2
      | otherwise -> That                      n
    (Just n1, Just n2)
      | f x       -> These (insertSetMin x s1) n2
      | otherwise -> These n1                  (insertSetMin x s2)
  where
    (s1, s2) = S.partition f s0
{-# INLINABLE partition #-}

split
    :: Ord a
    => a
    -> NESet a
    -> Maybe (These (NESet a) (NESet a))
split x n@(NESet x0 s0) = case compare x x0 of
    LT -> Just $ That n
    EQ -> That <$> nonEmptySet s0
    GT -> case (nonEmptySet s1, nonEmptySet s2) of
      (Nothing, Nothing) -> Just $ This  (singleton x0)
      (Just _ , Nothing) -> Just $ This  (insertSetMin x0 s1)
      (Nothing, Just n2) -> Just $ These (singleton x0)       n2
      (Just _ , Just n2) -> Just $ These (insertSetMin x0 s1) n2
  where
    (s1, s2) = S.split x s0
{-# INLINABLE split #-}

splitMember
    :: Ord a
    => a
    -> NESet a
    -> (Bool, Maybe (These (NESet a) (NESet a)))
splitMember x n@(NESet x0 s0) = case compare x x0 of
    LT -> (False, Just $ That n)
    EQ -> (True , That <$> nonEmptySet s0)
    GT -> (mem  ,) $ case (nonEmptySet s1, nonEmptySet s2) of
      (Nothing, Nothing) -> Just $ This  (singleton x0)
      (Just _ , Nothing) -> Just $ This  (insertSetMin x0 s1)
      (Nothing, Just n2) -> Just $ These (singleton x0)       n2
      (Just _ , Just n2) -> Just $ These (insertSetMin x0 s1) n2
  where
    (s1, mem, s2) = S.splitMember x s0
{-# INLINABLE splitMember #-}

splitRoot
    :: NESet a
    -> NonEmpty (NESet a)
splitRoot (NESet x s) = singleton x
                     :| mapMaybe nonEmptySet (S.splitRoot s)
{-# INLINE splitRoot #-}

lookupIndex
    :: Ord a
    => a
    -> NESet a
    -> Maybe Int
lookupIndex x (NESet x0 s) = case compare x x0 of
    LT -> Nothing
    EQ -> Just 0
    GT -> (+ 1) <$> S.lookupIndex x s
{-# INLINE lookupIndex #-}

findIndex
    :: Ord a
    => a
    -> NESet a
    -> Int
findIndex k = fromMaybe e . lookupIndex k
  where
    e = error "NESet.findIndex: element is not in the set"
{-# INLINE findIndex #-}

elemAt
    :: Int
    -> NESet a
    -> a
elemAt 0 (NESet x _) = x
elemAt i (NESet _ s) = S.elemAt (i - 1) s
{-# INLINE elemAt #-}

deleteAt
    :: Int
    -> NESet a
    -> Set a
deleteAt 0 (NESet _ s) = s
deleteAt i (NESet x s) = insertMinSet x . S.deleteAt (i - 1) $ s

take
    :: Int
    -> NESet a
    -> Set a
take 0 (NESet _ _) = S.empty
take i (NESet x s) = insertMinSet x . S.take (i - 1) $ s
{-# INLINABLE take #-}

drop
    :: Int
    -> NESet a
    -> Set a
drop 0 n           = toSet n
drop n (NESet _ s) = S.drop (n - 1) s
{-# INLINABLE drop #-}

splitAt
    :: Int
    -> NESet a
    -> These (NESet a) (NESet a)
splitAt 0 n              = That n
splitAt i n@(NESet x s0) = case (nonEmptySet s1, nonEmptySet s2) of
    (Nothing, Nothing) -> This  (singleton x)
    (Just _ , Nothing) -> This  n
    (Nothing, Just n2) -> These (singleton x)       n2
    (Just _ , Just n2) -> These (insertSetMin x s1) n2
  where
    (s1, s2) = S.splitAt (i - 1) s0
{-# INLINABLE splitAt #-}

map :: Ord b
    => (a -> b)
    -> NESet a
    -> NESet b
map f (NESet x0 s) = fromList
                   . (f x0 :|)
                   . S.foldr (\x xs -> f x : xs) []
                   $ s
{-# INLINE map #-}


mapMonotonic
    :: (a -> b)
    -> NESet a
    -> NESet b
mapMonotonic f (NESet x s) = NESet (f x) (S.mapMonotonic f s)
{-# INLINE mapMonotonic #-}

foldr1' :: (a -> a -> a) -> NESet a -> a
foldr1' f (NESet x s) = case S.maxView s of
    Nothing      -> x
    Just (y, s') -> let !z = S.foldr' f y s' in x `f` z
{-# INLINE foldr1' #-}

foldl1' :: (a -> a -> a) -> NESet a -> a
foldl1' f (NESet x s) = S.foldl' f x s
{-# INLINE foldl1' #-}

findMin :: NESet a -> a
findMin (NESet x _) = x
{-# INLINE findMin #-}

findMax :: NESet a -> a
findMax (NESet x s) = fromMaybe x . S.lookupMax $ s
{-# INLINE findMax #-}

deleteMin :: NESet a -> Set a
deleteMin (NESet _ s) = s
{-# INLINE deleteMin #-}

deleteMax :: NESet a -> Set a
deleteMax (NESet x s) = insertMinSet x . S.deleteMax $ s
{-# INLINE deleteMax #-}

deleteFindMin :: NESet a -> (a, Set a)
deleteFindMin (NESet x s) = (x, s)
{-# INLINE deleteFindMin #-}

deleteFindMax :: NESet a -> (a, Set a)
deleteFindMax (NESet x s) = maybe (x, S.empty) (second (insertMinSet x))
                          . S.maxView
                          $ s
{-# INLINE deleteFindMax #-}

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
