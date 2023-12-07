{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- |
-- Module      : Data.Set.NonEmpty
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- = Non-Empty Finite Sets
--
-- The @'NESet' e@ type represents a non-empty set of elements of type @e@.
-- Most operations require that @e@ be an instance of the 'Ord' class.
-- A 'NESet' is strict in its elements.
--
-- See documentation for 'NESet' for information on how to convert and
-- manipulate such non-empty set.
--
-- This module essentially re-imports the API of "Data.Set" and its 'Set'
-- type, along with semantics and asymptotics.  In most situations,
-- asymptotics are different only by a constant factor.  In some
-- situations, asmyptotics are even better (constant-time instead of
-- log-time).  All typeclass constraints are identical to their "Data.Set"
-- counterparts.
--
-- Because 'NESet' is implemented using 'Set', all of the caveats of using
-- 'Set' apply (such as the limitation of the maximum size of sets).
--
-- All functions take non-empty sets as inputs.  In situations where their
-- results can be guarunteed to also be non-empty, they also return
-- non-empty sets.  In situations where their results could potentially be
-- empty, 'Set' is returned instead.
--
-- Some functions ('partition', 'spanAntitone', 'split') have modified
-- return types to account for possible configurations of non-emptiness.
--
-- This module is intended to be imported qualified, to avoid name clashes
-- with "Prelude" and "Data.Set" functions:
--
-- > import qualified Data.Set.NonEmpty as NES
module Data.Set.NonEmpty (
  -- * Non-Empty Set Type
    NESet
  -- ** Conversions between empty and non-empty sets
  , pattern IsNonEmpty
  , pattern IsEmpty
  , nonEmptySet
  , toSet
  , withNonEmpty
  , insertSet
  , insertSetMin
  , insertSetMax
  , unsafeFromSet

  -- * Construction
  , singleton
  , fromList
  , fromAscList
  , fromDescList
  , fromDistinctAscList
  , fromDistinctDescList
  , powerSet

  -- * Insertion
  , insert

  -- * Deletion
  , delete

  -- * Query
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

  -- * Combine
  , union
  , unions
  , difference
  , (\\)
  , intersection
  , cartesianProduct
  , disjointUnion

  -- * Filter
  , filter
  , takeWhileAntitone
  , dropWhileAntitone
  , spanAntitone
  , partition
  , split
  , splitMember
  , splitRoot

  -- * Indexed
  , lookupIndex
  , findIndex
  , elemAt
  , deleteAt
  , take
  , drop
  , splitAt

  -- * Map
  , map
  , mapMonotonic

  -- * Folds
  , foldr
  , foldl
  , F.foldr1
  , F.foldl1
  -- ** Strict folds
  , foldr'
  , foldl'
  , foldr1'
  , foldl1'

  -- * Min\/Max
  , findMin
  , findMax
  , deleteMin
  , deleteMax
  , deleteFindMin
  , deleteFindMax

  -- * Conversion

  -- ** List
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
import           Prelude hiding             (Foldable(..), filter, map, take, drop, splitAt)
import qualified Data.Foldable              as F
import qualified Data.List.NonEmpty         as NE
import qualified Data.Semigroup.Foldable    as F1
import qualified Data.Set                   as S

-- | /O(1)/ match, /O(log n)/ usage of contents. The 'IsNonEmpty' and
-- 'IsEmpty' patterns allow you to treat a 'Set' as if it were either
-- a @'IsNonEmpty' n@ (where @n@ is a 'NESet') or an 'IsEmpty'.
--
-- For example, you can pattern match on a 'Set':
--
-- @
-- myFunc :: 'Set' X -> Y
-- myFunc ('IsNonEmpty' n) =  -- here, the user provided a non-empty set, and @n@ is the 'NESet'
-- myFunc 'IsEmpty'        =  -- here, the user provided an empty set
-- @
--
-- Matching on @'IsNonEmpty' n@ means that the original 'Set' was /not/
-- empty, and you have a verified-non-empty 'NESet' @n@ to use.
--
-- Note that patching on this pattern is /O(1)/.  However, using the
-- contents requires a /O(log n)/ cost that is deferred until after the
-- pattern is matched on (and is not incurred at all if the contents are
-- never used).
--
-- A case statement handling both 'IsNonEmpty' and 'IsEmpty' provides
-- complete coverage.
--
-- This is a bidirectional pattern, so you can use 'IsNonEmpty' to convert
-- a 'NESet' back into a 'Set', obscuring its non-emptiness (see 'toSet').
pattern IsNonEmpty :: NESet a -> Set a
pattern IsNonEmpty n <- (nonEmptySet->Just n)
  where
    IsNonEmpty n = toSet n

-- | /O(1)/. The 'IsNonEmpty' and 'IsEmpty' patterns allow you to treat
-- a 'Set' as if it were either a @'IsNonEmpty' n@ (where @n@ is
-- a 'NESet') or an 'IsEmpty'.
--
-- Matching on 'IsEmpty' means that the original 'Set' was empty.
--
-- A case statement handling both 'IsNonEmpty' and 'IsEmpty' provides
-- complete coverage.
--
-- This is a bidirectional pattern, so you can use 'IsEmpty' as an
-- expression, and it will be interpreted as 'Data.Set.empty'.
--
-- See 'IsNonEmpty' for more information.
pattern IsEmpty :: Set a
pattern IsEmpty <- (S.null->True)
  where
    IsEmpty = S.empty

{-# COMPLETE IsNonEmpty, IsEmpty #-}

-- | /O(log n)/. Unsafe version of 'nonEmptySet'.  Coerces a 'Set' into an
-- 'NESet', but is undefined (throws a runtime exception when evaluation is
-- attempted) for an empty 'Set'.
unsafeFromSet
    :: Set a
    -> NESet a
unsafeFromSet = withNonEmpty e id
  where
    e = errorWithoutStackTrace "NESet.unsafeFromSet: empty set"
{-# INLINE unsafeFromSet #-}

-- | /O(log n)/. Convert a 'Set' into an 'NESet' by adding a value.
-- Because of this, we know that the set must have at least one
-- element, and so therefore cannot be empty.
--
-- See 'insertSetMin' for a version that is constant-time if the new value is
-- /strictly smaller than/ all values in the original set
--
-- > insertSet 4 (Data.Set.fromList [5, 3]) == fromList (3 :| [4, 5])
-- > insertSet 4 Data.Set.empty == singleton 4 "c"
insertSet :: Ord a => a -> Set a -> NESet a
insertSet x = withNonEmpty (singleton x) (insert x)
{-# INLINE insertSet #-}

-- | /O(1)/ Convert a 'Set' into an 'NESet' by adding a value where the
-- value is /strictly less than/ all values in the input set  The values in
-- the original map must all be /strictly greater than/ the new value.
-- /The precondition is not checked./
--
-- > insertSetMin 2 (Data.Set.fromList [5, 3]) == fromList (2 :| [3, 5])
-- > valid (insertSetMin 2 (Data.Set.fromList [5, 3])) == True
-- > valid (insertSetMin 7 (Data.Set.fromList [5, 3])) == False
-- > valid (insertSetMin 3 (Data.Set.fromList [5, 3])) == False
insertSetMin :: a -> Set a -> NESet a
insertSetMin = NESet
{-# INLINE insertSetMin #-}

-- | /O(log n)/ Convert a 'Set' into an 'NESet' by adding a value where the
-- value is /strictly less than/ all values in the input set  The values in
-- the original map must all be /strictly greater than/ the new value.
-- /The precondition is not checked./
--
-- While this has the same asymptotics as 'insertSet', it saves a constant
-- factor for key comparison (so may be helpful if comparison is expensive)
-- and also does not require an 'Ord' instance for the key type.
--
-- > insertSetMin 7 (Data.Set.fromList [5, 3]) == fromList (3 :| [5, 7])
-- > valid (insertSetMin 7 (Data.Set.fromList [5, 3])) == True
-- > valid (insertSetMin 2 (Data.Set.fromList [5, 3])) == False
-- > valid (insertSetMin 5 (Data.Set.fromList [5, 3])) == False
insertSetMax :: a -> Set a -> NESet a
insertSetMax x = withNonEmpty (singleton x) go
  where
    go (NESet x0 s0) = NESet x0 . insertMaxSet x $ s0
{-# INLINE insertSetMax #-}

-- | /O(n)/. Build a set from an ascending list in linear time.  /The
-- precondition (input list is ascending) is not checked./
fromAscList :: Eq a => NonEmpty a -> NESet a
fromAscList = fromDistinctAscList . combineEq
{-# INLINE fromAscList #-}

-- | /O(n)/. Build a set from an ascending list of distinct elements in linear time.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: NonEmpty a -> NESet a
fromDistinctAscList (x :| xs) = insertSetMin x
                              . S.fromDistinctAscList
                              $ xs
{-# INLINE fromDistinctAscList #-}

-- | /O(n)/. Build a set from a descending list in linear time.
-- /The precondition (input list is descending) is not checked./
fromDescList :: Eq a => NonEmpty a -> NESet a
fromDescList = fromDistinctDescList . combineEq
{-# INLINE fromDescList #-}

-- | /O(n)/. Build a set from a descending list of distinct elements in linear time.
-- /The precondition (input list is strictly descending) is not checked./
fromDistinctDescList :: NonEmpty a -> NESet a
fromDistinctDescList (x :| xs) = insertSetMax x
                               . S.fromDistinctDescList
                               $ xs
{-# INLINE fromDistinctDescList #-}

-- | Calculate the power set of a non-empty: the set of all its (non-empty)
-- subsets.
--
-- @
-- t ``member`` powerSet s == t ``isSubsetOf`` s
-- @
--
-- Example:
--
-- @
-- powerSet (fromList (1 :| [2,3])) =
--   fromList (singleton 1 :| [ singleton 2
--                            , singleton 3
--                            , fromList (1 :| [2])
--                            , fromList (1 :| [3])
--                            , fromList (2 :| [3])
--                            , fromList (1 :| [2,3])
--                            ]
--            )
-- @
--
-- We know that the result is non-empty because the result will always at
-- least contain the original set.
powerSet
    :: forall a. ()
    => NESet a
    -> NESet (NESet a)
powerSet (NESet x s0) = case nonEmptySet p1 of
    -- s0 was empty originally
    Nothing -> singleton (singleton x)
    -- s1 was not empty originally
    Just p2 -> mapMonotonic (insertSetMin x) p0
       `merge` p2
  where
    -- powerset should never be empty
    p0 :: NESet (Set a)
    p0@(NESet _ p0s) = forSure $ powerSetSet s0
    p1 :: Set (NESet a)
    p1 = S.mapMonotonic forSure p0s  -- only minimal element is empty, so the rest aren't
    forSure = withNonEmpty (errorWithoutStackTrace "NESet.powerSet: internal error")
                        id
{-# INLINABLE powerSet #-}

-- | /O(log n)/. Insert an element in a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.
insert :: Ord a => a -> NESet a -> NESet a
insert x n@(NESet x0 s) = case compare x x0 of
    LT -> NESet x  $ toSet n
    EQ -> NESet x  s
    GT -> NESet x0 $ S.insert x s
{-# INLINE insert #-}

-- | /O(log n)/. Delete an element from a set.
delete :: Ord a => a -> NESet a -> Set a
delete x n@(NESet x0 s) = case compare x x0 of
    LT -> toSet n
    EQ -> s
    GT -> insertMinSet x0 . S.delete x $ s
{-# INLINE delete #-}

-- | /O(log n)/. Is the element in the set?
member :: Ord a => a -> NESet a -> Bool
member x (NESet x0 s) = case compare x x0 of
    LT -> False
    EQ -> True
    GT -> S.member x s
{-# INLINE member #-}

-- | /O(log n)/. Is the element not in the set?
notMember :: Ord a => a -> NESet a -> Bool
notMember x (NESet x0 s) = case compare x x0 of
    LT -> True
    EQ -> False
    GT -> S.notMember x s
{-# INLINE notMember #-}

-- | /O(log n)/. Find largest element smaller than the given one.
--
-- > lookupLT 3 (fromList (3 :| [5])) == Nothing
-- > lookupLT 5 (fromList (3 :| [5])) == Just 3
lookupLT :: Ord a => a -> NESet a -> Maybe a
lookupLT x (NESet x0 s) = case compare x x0 of
    LT -> Nothing
    EQ -> Nothing
    GT -> S.lookupLT x s <|> Just x0
{-# INLINE lookupLT #-}

-- | /O(log n)/. Find smallest element greater than the given one.
--
-- > lookupLT 4 (fromList (3 :| [5])) == Just 5
-- > lookupLT 5 (fromList (3 :| [5])) == Nothing
lookupGT :: Ord a => a -> NESet a -> Maybe a
lookupGT x (NESet x0 s) = case compare x x0 of
    LT -> Just x0
    EQ -> S.lookupMin s
    GT -> S.lookupGT x s
{-# INLINE lookupGT #-}

-- | /O(log n)/. Find largest element smaller or equal to the given one.
--
-- > lookupLT 2 (fromList (3 :| [5])) == Nothing
-- > lookupLT 4 (fromList (3 :| [5])) == Just 3
-- > lookupLT 5 (fromList (3 :| [5])) == Just 5
lookupLE :: Ord a => a -> NESet a -> Maybe a
lookupLE x (NESet x0 s) = case compare x x0 of
    LT -> Nothing
    EQ -> Just x0
    GT -> S.lookupLE x s <|> Just x0
{-# INLINE lookupLE #-}

-- | /O(log n)/. Find smallest element greater or equal to the given one.
--
-- > lookupLT 3 (fromList (3 :| [5])) == Just 3
-- > lookupLT 4 (fromList (3 :| [5])) == Just 5
-- > lookupLT 6 (fromList (3 :| [5])) == Nothing
lookupGE :: Ord a => a -> NESet a -> Maybe a
lookupGE x (NESet x0 s) = case compare x x0 of
    LT -> Just x0
    EQ -> Just x0
    GT -> S.lookupGE x s
{-# INLINE lookupGE #-}

-- | /O(n+m)/. Is this a subset?
-- @(s1 \`isSubsetOf\` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf
    :: Ord a
    => NESet a
    -> NESet a
    -> Bool
isSubsetOf (NESet x s0) (toSet->s1) = x `S.member` s1
                                   && s0 `S.isSubsetOf` s1
{-# INLINE isSubsetOf #-}

-- | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf
    :: Ord a
    => NESet a
    -> NESet a
    -> Bool
isProperSubsetOf s0 s1 = S.size (nesSet s0) < S.size (nesSet s1)
                      && s0 `isSubsetOf` s1
{-# INLINE isProperSubsetOf #-}

-- | /O(n+m)/. Check whether two sets are disjoint (i.e. their intersection
--   is empty).
--
-- > disjoint (fromList (2:|[4,6]))   (fromList (1:|[3]))     == True
-- > disjoint (fromList (2:|[4,6,8])) (fromList (2:|[3,5,7])) == False
-- > disjoint (fromList (1:|[2]))     (fromList (1:|[2,3,4])) == False
disjoint
    :: Ord a
    => NESet a
    -> NESet a
    -> Bool
disjoint n1@(NESet x1 s1) n2@(NESet x2 s2) = case compare x1 x2 of
    -- x1 is not in n2
    LT -> s1 `disjointSet` toSet n2
    -- k1 and k2 are a part of the result
    EQ -> False
    -- k2 is not in n1
    GT -> toSet n1 `disjointSet` s2
{-# INLINE disjoint #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Difference of two sets.
--
-- Returns a potentially empty set ('Set') because the first set might be
-- a subset of the second set, and therefore have all of its elements
-- removed.
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

-- | /O(m*log(n\/m + 1)), m <= n/. The intersection of two sets.
--
-- Returns a potentially empty set ('Set'), because the two sets might have
-- an empty intersection.
--
-- Elements of the result come from the first set, so for example
--
-- > import qualified Data.Set.NonEmpty as NES
-- > data AB = A | B deriving Show
-- > instance Ord AB where compare _ _ = EQ
-- > instance Eq AB where _ == _ = True
-- > main = print (NES.singleton A `NES.intersection` NES.singleton B,
-- >               NES.singleton B `NES.intersection` NES.singleton A)
--
-- prints @(fromList (A:|[]),fromList (B:|[]))@.
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

-- | Calculate the Cartesian product of two sets.
--
-- @
-- cartesianProduct xs ys = fromList $ liftA2 (,) (toList xs) (toList ys)
-- @
--
-- Example:
--
-- @
-- cartesianProduct (fromList (1:|[2])) (fromList (\'a\':|[\'b\'])) =
--   fromList ((1,\'a\') :| [(1,\'b\'), (2,\'a\'), (2,\'b\')])
-- @
cartesianProduct
    :: NESet a
    -> NESet b
    -> NESet (a, b)
cartesianProduct n1 n2 = getMergeNESet
                       . F1.foldMap1 (\x -> MergeNESet $ mapMonotonic (x,) n2)
                       $ n1
{-# INLINE cartesianProduct #-}

-- | Calculate the disjoint union of two sets.
--
-- @ disjointUnion xs ys = map Left xs ``union`` map Right ys @
--
-- Example:
--
-- @
-- disjointUnion (fromList (1:|[2])) (fromList ("hi":|["bye"])) =
--   fromList (Left 1 :| [Left 2, Right "hi", Right "bye"])
-- @
disjointUnion
    :: NESet a
    -> NESet b
    -> NESet (Either a b)
disjointUnion (NESet x1 s1) n2 = NESet (Left x1)
                                       (s1 `disjointUnionSet` toSet n2)
{-# INLINE disjointUnion #-}

-- | /O(n)/. Filter all elements that satisfy the predicate.
--
-- Returns a potentially empty set ('Set') because the predicate might
-- filter out all items in the original non-empty set.
filter
    :: (a -> Bool)
    -> NESet a
    -> Set a
filter f (NESet x s1)
    | f x       = insertMinSet x . S.filter f $ s1
    | otherwise = S.filter f s1
{-# INLINE filter #-}

-- | /O(log n)/. Take while a predicate on the elements holds.  The user is
-- responsible for ensuring that for all elements @j@ and @k@ in the set,
-- @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.
--
-- Returns a potentially empty set ('Set') because the predicate might fail
-- on the first input.
--
-- @
-- takeWhileAntitone p = Data.Set.fromDistinctAscList . Data.List.NonEmpty.takeWhile p . 'toList'
-- takeWhileAntitone p = 'filter' p
-- @
takeWhileAntitone
    :: (a -> Bool)
    -> NESet a
    -> Set a
takeWhileAntitone f (NESet x s)
    | f x       = insertMinSet x . S.takeWhileAntitone f $ s
    | otherwise = S.empty
{-# INLINE takeWhileAntitone #-}

-- | /O(log n)/. Drop while a predicate on the elements holds.  The user is
-- responsible for ensuring that for all elements @j@ and @k@ in the set,
-- @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.
--
-- Returns a potentially empty set ('Set') because the predicate might be
-- true for all items.
--
-- @
-- dropWhileAntitone p = Data.Set.fromDistinctAscList . Data.List.NonEmpty.dropWhile p . 'toList'
-- dropWhileAntitone p = 'filter' (not . p)
-- @
dropWhileAntitone
    :: (a -> Bool)
    -> NESet a
    -> Set a
dropWhileAntitone f n@(NESet x s)
    | f x       = S.dropWhileAntitone f s
    | otherwise = toSet n
{-# INLINE dropWhileAntitone #-}

-- | /O(log n)/. Divide a set at the point where a predicate on the
-- elements stops holding.  The user is responsible for ensuring that for
-- all elements @j@ and @k@ in the set, @j \< k ==\> p j \>= p k@.
--
-- Returns a 'These' with potentially two non-empty sets:
--
-- *   @'This' n1@ means that the predicate never failed for any item,
--     returning the original set
-- *   @'That' n2@ means that the predicate failed for the first item,
--     returning the original set
-- *   @'These' n1 n2@ gives @n1@ (the set up to the point where the
--     predicate stops holding) and @n2@ (the set starting from
--     the point where the predicate stops holding)
--
-- @
-- spanAntitone p xs = partition p xs
-- @
--
-- Note: if @p@ is not actually antitone, then @spanAntitone@ will split the set
-- at some /unspecified/ point where the predicate switches from holding to not
-- holding (where the predicate is seen to hold before the first element and to fail
-- after the last element).
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
{-# INLINABLE spanAntitone #-}

-- | /O(n)/. Partition the map according to a predicate.
--
-- Returns a 'These' with potentially two non-empty sets:
--
-- *   @'This' n1@ means that the predicate was true for all items.
-- *   @'That' n2@ means that the predicate was false for all items.
-- *   @'These' n1 n2@ gives @n1@ (all of the items that were true for the
--     predicate) and @n2@ (all of the items that were false for the
--     predicate).
--
-- See also 'split'.
--
-- > partition (> 3) (fromList (5 :| [3])) == These (singleton 5) (singleton 3)
-- > partition (< 7) (fromList (5 :| [3])) == This  (fromList (3 :| [5]))
-- > partition (> 7) (fromList (5 :| [3])) == That  (fromList (3 :| [5]))
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

-- | /O(log n)/. The expression (@'split' x set@) is potentially a 'These'
-- containing up to two 'NESet's based on splitting the set into sets
-- containing items before and after the value @x@.  It will never return
-- a set that contains @x@ itself.
--
-- *   'Nothing' means that @x@ was the only value in the the original set,
--     and so there are no items before or after it.
-- *   @'Just' ('This' n1)@ means @x@ was larger than or equal to all items
--     in the set, and @n1@ is the entire original set (minus @x@, if it
--     was present)
-- *   @'Just' ('That' n2)@ means @x@ was smaller than or equal to all
--     items in the set, and @n2@ is the entire original set (minus @x@, if
--     it was present)
-- *   @'Just' ('These' n1 n2)@ gives @n1@ (the set of all values from the
--     original set less than @x@) and @n2@ (the set of all values from the
--     original set greater than @x@).
--
-- > split 2 (fromList (5 :| [3])) == Just (That  (fromList (3 :| [5]))      )
-- > split 3 (fromList (5 :| [3])) == Just (That  (singleton 5)              )
-- > split 4 (fromList (5 :| [3])) == Just (These (singleton 3) (singleton 5))
-- > split 5 (fromList (5 :| [3])) == Just (This  (singleton 3)              )
-- > split 6 (fromList (5 :| [3])) == Just (This  (fromList (3 :| [5]))      )
-- > split 5 (singleton 5)         == Nothing
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

-- | /O(log n)/. The expression (@'splitMember' x set@) splits a set just
-- like 'split' but also returns @'member' x set@ (whether or not @x@ was
-- in @set@)
--
-- > splitMember 2 (fromList (5 :| [3])) == (False, Just (That  (fromList (3 :| [5)]))))
-- > splitMember 3 (fromList (5 :| [3])) == (True , Just (That  (singleton 5)))
-- > splitMember 4 (fromList (5 :| [3])) == (False, Just (These (singleton 3) (singleton 5)))
-- > splitMember 5 (fromList (5 :| [3])) == (True , Just (This  (singleton 3))
-- > splitMember 6 (fromList (5 :| [3])) == (False, Just (This  (fromList (3 :| [5])))
-- > splitMember 5 (singleton 5)         == (True , Nothing)
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

-- | /O(1)/.  Decompose a set into pieces based on the structure of the underlying
-- tree.  This function is useful for consuming a set in parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that
-- the pieces returned will be in ascending order (all elements in the
-- first subset less than all elements in the second, and so on).
--
--  Note that the current implementation does not return more than four
--  subsets, but you should not depend on this behaviour because it can
--  change in the future without notice.
splitRoot
    :: NESet a
    -> NonEmpty (NESet a)
splitRoot (NESet x s) = singleton x
                     :| mapMaybe nonEmptySet (S.splitRoot s)
{-# INLINE splitRoot #-}

-- | /O(log n)/. Lookup the /index/ of an element, which is its zero-based
-- index in the sorted sequence of elements. The index is a number from /0/
-- up to, but not including, the 'size' of the set.
--
-- > isJust   (lookupIndex 2 (fromList (5:|[3]))) == False
-- > fromJust (lookupIndex 3 (fromList (5:|[3]))) == 0
-- > fromJust (lookupIndex 5 (fromList (5:|[3]))) == 1
-- > isJust   (lookupIndex 6 (fromList (5:|[3]))) == False
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

-- | /O(log n)/. Return the /index/ of an element, which is its zero-based
-- index in the sorted sequence of elements. The index is a number from /0/
-- up to, but not including, the 'size' of the set. Calls 'error' when the
-- element is not a 'member' of the set.
--
-- > findIndex 2 (fromList (5:|[3]))    Error: element is not in the set
-- > findIndex 3 (fromList (5:|[3])) == 0
-- > findIndex 5 (fromList (5:|[3])) == 1
-- > findIndex 6 (fromList (5:|[3]))    Error: element is not in the set
findIndex
    :: Ord a
    => a
    -> NESet a
    -> Int
findIndex k = fromMaybe e . lookupIndex k
  where
    e = error "NESet.findIndex: element is not in the set"
{-# INLINE findIndex #-}

-- | /O(log n)/. Retrieve an element by its /index/, i.e. by its zero-based
-- index in the sorted sequence of elements. If the /index/ is out of range
-- (less than zero, greater or equal to 'size' of the set), 'error' is
-- called.
--
-- > elemAt 0 (fromList (5:|[3])) == 3
-- > elemAt 1 (fromList (5:|[3])) == 5
-- > elemAt 2 (fromList (5:|[3]))    Error: index out of range
elemAt
    :: Int
    -> NESet a
    -> a
elemAt 0 (NESet x _) = x
elemAt i (NESet _ s) = S.elemAt (i - 1) s
{-# INLINE elemAt #-}

-- | /O(log n)/. Delete the element at /index/, i.e. by its zero-based
-- index in the sorted sequence of elements. If the /index/ is out of range
-- (less than zero, greater or equal to 'size' of the set), 'error' is
-- called.
--
-- Returns a potentially empty set ('Set'), because this could potentailly
-- delete the final element in a singleton set.
--
-- > deleteAt 0    (fromList (5:|[3])) == singleton 5
-- > deleteAt 1    (fromList (5:|[3])) == singleton 3
-- > deleteAt 2    (fromList (5:|[3]))    Error: index out of range
-- > deleteAt (-1) (fromList (5:|[3]))    Error: index out of range
deleteAt
    :: Int
    -> NESet a
    -> Set a
deleteAt 0 (NESet _ s) = s
deleteAt i (NESet x s) = insertMinSet x . S.deleteAt (i - 1) $ s
{-# INLINABLE deleteAt #-}

-- | Take a given number of elements in order, beginning
-- with the smallest ones.
--
-- Returns a potentailly empty set ('Set'), which can only happen when
-- calling @take 0@.
--
-- @
-- take n = Data.Set.fromDistinctAscList . Data.List.NonEmpty.take n . 'toAscList'
-- @
take
    :: Int
    -> NESet a
    -> Set a
take 0 (NESet _ _) = S.empty
take i (NESet x s) = insertMinSet x . S.take (i - 1) $ s
{-# INLINABLE take #-}

-- | Drop a given number of elements in order, beginning
-- with the smallest ones.
--
-- Returns a potentailly empty set ('Set'), in the case that 'drop' is
-- called with a number equal to or greater the number of items in the set,
-- and we drop every item.
--
-- @
-- drop n = Data.Set.fromDistinctAscList . Data.List.NonEmpty.drop n . 'toAscList'
-- @
drop
    :: Int
    -> NESet a
    -> Set a
drop 0 n           = toSet n
drop n (NESet _ s) = S.drop (n - 1) s
{-# INLINABLE drop #-}

-- | /O(log n)/. Split a set at a particular index @i@.
--
-- *   @'This' n1@ means that there are less than @i@ items in the set, and
--     @n1@ is the original set.
-- *   @'That' n2@ means @i@ was 0; we dropped 0 items, so @n2@ is the
--     original set.
-- *   @'These' n1 n2@ gives @n1@ (taking @i@ items from the original set)
--     and @n2@ (dropping @i@ items from the original set))
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

-- | /O(n*log n)/.
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@
map :: Ord b
    => (a -> b)
    -> NESet a
    -> NESet b
map f (NESet x0 s) = fromList
                   . (f x0 :|)
                   . S.foldr (\x xs -> f x : xs) []
                   $ s
{-# INLINE map #-}

-- | /O(n)/.
-- @'mapMonotonic' f s == 'map' f s@, but works only when @f@ is strictly
-- increasing.  /The precondition is not checked./ Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapMonotonic f s == map f s
-- >     where ls = Data.Foldable.toList s
mapMonotonic
    :: (a -> b)
    -> NESet a
    -> NESet b
mapMonotonic f (NESet x s) = NESet (f x) (S.mapMonotonic f s)
{-# INLINE mapMonotonic #-}

-- | /O(n)/. A strict version of 'foldr1'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr1' :: (a -> a -> a) -> NESet a -> a
foldr1' f (NESet x s) = case S.maxView s of
    Nothing      -> x
    Just (y, s') -> let !z = S.foldr' f y s' in x `f` z
{-# INLINE foldr1' #-}

-- | /O(n)/. A strict version of 'foldl1'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl1' :: (a -> a -> a) -> NESet a -> a
foldl1' f (NESet x s) = S.foldl' f x s
{-# INLINE foldl1' #-}

-- | /O(1)/. The minimal element of a set.  Note that this is total, making
-- 'Data.Set.lookupMin' obsolete.  It is constant-time, so has better
-- asymptotics than @Data.Set.lookupMin@ and @Data.Map.findMin@ as well.
--
-- > findMin (fromList (5 :| [3])) == 3
findMin :: NESet a -> a
findMin (NESet x _) = x
{-# INLINE findMin #-}

-- | /O(log n)/. The maximal key of a set  Note that this is total,
-- making 'Data.Set.lookupMin' obsolete.
--
-- > findMax (fromList (5 :| [3])) == 5
findMax :: NESet a -> a
findMax (NESet x s) = fromMaybe x . S.lookupMax $ s
{-# INLINE findMax #-}

-- | /O(1)/. Delete the minimal element.  Returns a potentially empty set
-- ('Set'), because we might delete the final item in a singleton set.  It
-- is constant-time, so has better asymptotics than @Data.Set.deleteMin@.
--
-- > deleteMin (fromList (5 :| [3, 7])) == Data.Set.fromList [5, 7]
-- > deleteMin (singleton 5) == Data.Set.empty
deleteMin :: NESet a -> Set a
deleteMin (NESet _ s) = s
{-# INLINE deleteMin #-}

-- | /O(log n)/. Delete the maximal element.  Returns a potentially empty
-- set ('Set'), because we might delete the final item in a singleton set.
--
-- > deleteMax (fromList (5 :| [3, 7])) == Data.Set.fromList [3, 5]
-- > deleteMax (singleton 5) == Data.Set.empty
deleteMax :: NESet a -> Set a
deleteMax (NESet x s) = case S.maxView s of
    Nothing      -> S.empty
    Just (_, s') -> insertMinSet x s'
{-# INLINE deleteMax #-}

-- | /O(1)/. Delete and find the minimal element.  It is constant-time, so
-- has better asymptotics that @Data.Set.minView@ for 'Set'.
--
-- Note that unlike @Data.Set.deleteFindMin@ for 'Set', this cannot ever
-- fail, and so is a total function. However, the result 'Set' is
-- potentially empty, since the original set might have contained just
-- a single item.
--
-- > deleteFindMin (fromList (5 :| [3, 10])) == (3, Data.Set.fromList [5, 10])
deleteFindMin :: NESet a -> (a, Set a)
deleteFindMin (NESet x s) = (x, s)
{-# INLINE deleteFindMin #-}

-- | /O(log n)/. Delete and find the minimal element.
--
-- Note that unlike @Data.Set.deleteFindMax@ for 'Set', this cannot ever
-- fail, and so is a total function. However, the result 'Set' is
-- potentially empty, since the original set might have contained just
-- a single item.
--
-- > deleteFindMax (fromList (5 :| [3, 10])) == (10, Data.Set.fromList [3, 5])
deleteFindMax :: NESet a -> (a, Set a)
deleteFindMax (NESet x s) = maybe (x, S.empty) (second (insertMinSet x))
                          . S.maxView
                          $ s
{-# INLINE deleteFindMax #-}

-- | /O(n)/. An alias of 'toAscList'. The elements of a set in ascending
-- order.
elems :: NESet a -> NonEmpty a
elems = toList
{-# INLINE elems #-}

-- | /O(n)/. Convert the set to an ascending non-empty list of elements.
toAscList :: NESet a -> NonEmpty a
toAscList = toList
{-# INLINE toAscList #-}

-- | /O(n)/. Convert the set to a descending non-empty list of elements.
toDescList :: NESet a -> NonEmpty a
toDescList (NESet x s) = S.foldl' (flip (NE.<|)) (x :| []) s
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
