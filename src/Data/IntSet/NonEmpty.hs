{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

-- |
-- Module      : Data.IntSet.NonEmpty
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- = Non-Empty Finite Integer Sets
--
-- The 'NEIntSet' type represents a non-empty set of integers.
--
-- See documentation for 'NEIntSet' for information on how to convert and
-- manipulate such non-empty set.
--
-- This module essentially re-imports the API of "Data.IntSet" and its 'IntSet'
-- type, along with semantics and asymptotics.  In most situations,
-- asymptotics are different only by a constant factor.  In some
-- situations, asmyptotics are even better (constant-time instead of
-- log-time).
--
-- Because 'NEIntSet' is implemented using 'IntSet', all of the caveats of
-- using 'IntSet' apply (such as the limitation of the maximum size of
-- sets).
--
-- All functions take non-empty sets as inputs.  In situations where their
-- results can be guarunteed to also be non-empty, they also return
-- non-empty sets.  In situations where their results could potentially be
-- empty, 'IntSet' is returned instead.
--
-- Some functions ('partition', 'split') have modified return types to
-- account for possible configurations of non-emptiness.
--
-- This module is intended to be imported qualified, to avoid name clashes
-- with "Prelude" and "Data.IntSet" functions:
--
-- > import qualified Data.IntSet.NonEmpty as NEIS
--
-- Note that all asmyptotics /O(f(n))/ in this module are actually
-- /O(min(W, f(n)))/, where @W@ is the number of bits in an 'Int' (32 or
-- 64).  That is, if @f(n)@ is greater than @W@, all operations are
-- constant-time.
module Data.IntSet.NonEmpty (
  -- * Non-Empty Set Type
    NEIntSet
  , Key

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
  , fromDistinctAscList

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

  -- * Filter
  , filter
  , partition
  , split
  , splitMember
  , splitRoot

  -- * Map
  , map

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
import           Data.IntSet                   (IntSet)
import           Data.IntSet.NonEmpty.Internal
import           Data.List.NonEmpty            (NonEmpty(..))
import           Data.Maybe
import           Data.These
import           Prelude hiding                (Foldable(..), filter, map)
import qualified Data.IntSet                   as S
import qualified Data.List.NonEmpty            as NE

-- | /O(1)/ match, /O(log n)/ usage of contents. The 'IsNonEmpty' and
-- 'IsEmpty' patterns allow you to treat a 'IntSet' as if it were either
-- a @'IsNonEmpty' n@ (where @n@ is a 'NEIntSet') or an 'IsEmpty'.
--
-- For example, you can pattern match on a 'IntSet':
--
-- @
-- myFunc :: 'IntSet' X -> Y
-- myFunc ('IsNonEmpty' n) =  -- here, the user provided a non-empty set, and @n@ is the 'NEIntSet'
-- myFunc 'IsEmpty'        =  -- here, the user provided an empty set
-- @
--
-- Matching on @'IsNonEmpty' n@ means that the original 'IntSet' was /not/
-- empty, and you have a verified-non-empty 'NEIntSet' @n@ to use.
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
-- a 'NEIntSet' back into a 'IntSet', obscuring its non-emptiness (see 'toSet').
pattern IsNonEmpty :: NEIntSet -> IntSet
pattern IsNonEmpty n <- (nonEmptySet->Just n)
  where
    IsNonEmpty n = toSet n

-- | /O(1)/. The 'IsNonEmpty' and 'IsEmpty' patterns allow you to treat
-- a 'IntSet' as if it were either a @'IsNonEmpty' n@ (where @n@ is
-- a 'NEIntSet') or an 'IsEmpty'.
--
-- Matching on 'IsEmpty' means that the original 'IntSet' was empty.
--
-- A case statement handling both 'IsNonEmpty' and 'IsEmpty' provides
-- complete coverage.
--
-- This is a bidirectional pattern, so you can use 'IsEmpty' as an
-- expression, and it will be interpreted as 'Data.IntSet.empty'.
--
-- See 'IsNonEmpty' for more information.
pattern IsEmpty :: IntSet
pattern IsEmpty <- (S.null->True)
  where
    IsEmpty = S.empty

{-# COMPLETE IsNonEmpty, IsEmpty #-}

-- | /O(log n)/. Convert a 'IntSet' into an 'NEIntSet' by adding a value.
-- Because of this, we know that the set must have at least one
-- element, and so therefore cannot be empty.
--
-- See 'insertSetMin' for a version that is constant-time if the new
-- value is /strictly smaller than/ all values in the original set
--
-- > insertSet 4 (Data.IntSet.fromList [5, 3]) == fromList (3 :| [4, 5])
-- > insertSet 4 Data.IntSet.empty == singleton 4 "c"
insertSet :: Key -> IntSet -> NEIntSet
insertSet x = withNonEmpty (singleton x) (insert x)
{-# INLINE insertSet #-}

-- | /O(1)/ Convert a 'IntSet' into an 'NEIntSet' by adding a value where the
-- value is /strictly less than/ all values in the input set  The values in
-- the original map must all be /strictly greater than/ the new value.
-- /The precondition is not checked./
--
-- > insertSetMin 2 (Data.IntSet.fromList [5, 3]) == fromList (2 :| [3, 5])
-- > valid (insertSetMin 2 (Data.IntSet.fromList [5, 3])) == True
-- > valid (insertSetMin 7 (Data.IntSet.fromList [5, 3])) == False
-- > valid (insertSetMin 3 (Data.IntSet.fromList [5, 3])) == False
insertSetMin :: Key -> IntSet -> NEIntSet
insertSetMin = NEIntSet
{-# INLINE insertSetMin #-}

-- | /O(log n)/ Convert a 'IntSet' into an 'NEIntSet' by adding a value
-- where the value is /strictly less than/ all values in the input set  The
-- values in the original map must all be /strictly greater than/ the new
-- value.  /The precondition is not checked./
--
-- At the current moment, this is identical simply 'insertSet'; however,
-- it is left both for consistency and as a placeholder for a future
-- version where optimizations are implemented to allow for a faster
-- implementation.
--
-- > insertSetMin 7 (Data.IntSet.fromList [5, 3]) == fromList (3 :| [5, 7])

-- these currently are all valid, but shouldn't be
-- > valid (insertSetMin 7 (Data.IntSet.fromList [5, 3])) == True
-- > valid (insertSetMin 2 (Data.IntSet.fromList [5, 3])) == False
-- > valid (insertSetMin 5 (Data.IntSet.fromList [5, 3])) == False
insertSetMax :: Key -> IntSet -> NEIntSet
insertSetMax x = withNonEmpty (singleton x) go
  where
    go (NEIntSet x0 s0) = NEIntSet x0 . insertMaxSet x $ s0
{-# INLINE insertSetMax #-}

-- | /O(log n)/. Unsafe version of 'nonEmptySet'.  Coerces a 'IntSet'
-- into an 'NEIntSet', but is undefined (throws a runtime exception when
-- evaluation is attempted) for an empty 'IntSet'.
unsafeFromSet
    :: IntSet
    -> NEIntSet
unsafeFromSet = withNonEmpty e id
  where
    e = errorWithoutStackTrace "NEIntSet.unsafeFromSet: empty set"
{-# INLINE unsafeFromSet #-}

-- | /O(n)/. Build a set from an ascending list in linear time.  /The
-- precondition (input list is ascending) is not checked./
fromAscList :: NonEmpty Key -> NEIntSet
fromAscList = fromDistinctAscList . combineEq
{-# INLINE fromAscList #-}

-- | /O(n)/. Build a set from an ascending list of distinct elements in linear time.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: NonEmpty Key -> NEIntSet
fromDistinctAscList (x :| xs) = insertSetMin x
                              . S.fromDistinctAscList
                              $ xs
{-# INLINE fromDistinctAscList #-}

-- | /O(log n)/. Insert an element in a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.
insert :: Key -> NEIntSet -> NEIntSet
insert x n@(NEIntSet x0 s) = case compare x x0 of
    LT -> NEIntSet x  $ toSet n
    EQ -> NEIntSet x  s
    GT -> NEIntSet x0 $ S.insert x s
{-# INLINE insert #-}

-- | /O(log n)/. Delete an element from a set.
delete :: Key -> NEIntSet -> IntSet
delete x n@(NEIntSet x0 s) = case compare x x0 of
    LT -> toSet n
    EQ -> s
    GT -> insertMinSet x0 . S.delete x $ s
{-# INLINE delete #-}

-- | /O(log n)/. Is the element in the set?
member :: Key -> NEIntSet -> Bool
member x (NEIntSet x0 s) = case compare x x0 of
    LT -> False
    EQ -> True
    GT -> S.member x s
{-# INLINE member #-}

-- | /O(log n)/. Is the element not in the set?
notMember :: Key -> NEIntSet -> Bool
notMember x (NEIntSet x0 s) = case compare x x0 of
    LT -> True
    EQ -> False
    GT -> S.notMember x s
{-# INLINE notMember #-}

-- | /O(log n)/. Find largest element smaller than the given one.
--
-- > lookupLT 3 (fromList (3 :| [5])) == Nothing
-- > lookupLT 5 (fromList (3 :| [5])) == Just 3
lookupLT :: Key -> NEIntSet -> Maybe Key
lookupLT x (NEIntSet x0 s) = case compare x x0 of
    LT -> Nothing
    EQ -> Nothing
    GT -> S.lookupLT x s <|> Just x0
{-# INLINE lookupLT #-}

-- | /O(log n)/. Find smallest element greater than the given one.
--
-- > lookupLT 4 (fromList (3 :| [5])) == Just 5
-- > lookupLT 5 (fromList (3 :| [5])) == Nothing
lookupGT :: Key -> NEIntSet -> Maybe Key
lookupGT x (NEIntSet x0 s) = case compare x x0 of
    LT -> Just x0
    EQ -> fst <$> S.minView s
    GT -> S.lookupGT x s
{-# INLINE lookupGT #-}

-- | /O(log n)/. Find largest element smaller or equal to the given one.
--
-- > lookupLT 2 (fromList (3 :| [5])) == Nothing
-- > lookupLT 4 (fromList (3 :| [5])) == Just 3
-- > lookupLT 5 (fromList (3 :| [5])) == Just 5
lookupLE :: Key -> NEIntSet -> Maybe Key
lookupLE x (NEIntSet x0 s) = case compare x x0 of
    LT -> Nothing
    EQ -> Just x0
    GT -> S.lookupLE x s <|> Just x0
{-# INLINE lookupLE #-}

-- | /O(log n)/. Find smallest element greater or equal to the given one.
--
-- > lookupLT 3 (fromList (3 :| [5])) == Just 3
-- > lookupLT 4 (fromList (3 :| [5])) == Just 5
-- > lookupLT 6 (fromList (3 :| [5])) == Nothing
lookupGE :: Key -> NEIntSet -> Maybe Key
lookupGE x (NEIntSet x0 s) = case compare x x0 of
    LT -> Just x0
    EQ -> Just x0
    GT -> S.lookupGE x s
{-# INLINE lookupGE #-}

-- | /O(n)/. Fold the elements in the set using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'Data.IntSet.NonEmpty.toAscList'@.
--
-- For example,
--
-- > elemsList set = foldr (:) [] set
foldr :: (Key -> b -> b) -> b -> NEIntSet -> b
foldr f z (NEIntSet x s) = x `f` S.foldr f z s
{-# INLINE foldr #-}

-- | /O(n)/. A strict version of 'foldr'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr' :: (Key -> b -> b) -> b -> NEIntSet -> b
foldr' f z (NEIntSet x s) = x `f` y
  where
    !y = S.foldr' f z s
{-# INLINE foldr' #-}

-- | /O(n)/. A version of 'foldr' that uses the value at the maximal value
-- in the set as the starting value.
--
-- Note that, unlike 'Data.Foldable.foldr1' for 'IntSet', this function is
-- total if the input function is total.
foldr1 :: (Key -> Key -> Key) -> NEIntSet -> Key
foldr1 f (NEIntSet x s) = maybe x (f x . uncurry (S.foldr f))
                        . S.maxView
                        $ s
{-# INLINE foldr1 #-}

-- | /O(n)/. Fold the elements in the set using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'Data.IntSet.NonEmpty.toAscList'@.
--
-- For example,
--
-- > descElemsList set = foldl (flip (:)) [] set
foldl :: (a -> Key -> a) -> a -> NEIntSet -> a
foldl f z (NEIntSet x s) = S.foldl f (f z x) s
{-# INLINE foldl #-}

-- | /O(n)/. A strict version of 'foldl'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl' :: (a -> Key -> a) -> a -> NEIntSet -> a
foldl' f z (NEIntSet x s) = S.foldl' f y s
  where
    !y = f z x
{-# INLINE foldl' #-}

-- | /O(n)/. A version of 'foldl' that uses the value at the minimal value
-- in the set as the starting value.
--
-- Note that, unlike 'Data.Foldable.foldl1' for 'IntSet', this function is
-- total if the input function is total.
foldl1 :: (Key -> Key -> Key) -> NEIntSet -> Key
foldl1 f (NEIntSet x s) = S.foldl f x s
{-# INLINE foldl1 #-}

-- | /O(n)/. A strict version of 'foldr1'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr1' :: (Key -> Key -> Key) -> NEIntSet -> Key
foldr1' f (NEIntSet x s) = case S.maxView s of
    Nothing      -> x
    Just (y, s') -> let !z = S.foldr' f y s' in x `f` z
{-# INLINE foldr1' #-}

-- | /O(n)/. A strict version of 'foldl1'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl1' :: (Key -> Key -> Key) -> NEIntSet -> Key
foldl1' f (NEIntSet x s) = S.foldl' f x s
{-# INLINE foldl1' #-}

-- | /O(1)/. The number of elements in the set.  Guaranteed to be greater
-- than zero.
size :: NEIntSet -> Int
size (NEIntSet _ s) = 1 + S.size s
{-# INLINE size #-}

-- | /O(n+m)/. Is this a subset?
-- @(s1 \`isSubsetOf\` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf
    :: NEIntSet
    -> NEIntSet
    -> Bool
isSubsetOf (NEIntSet x s0) (toSet->s1) = x `S.member` s1
                                         && s0 `S.isSubsetOf` s1
{-# INLINE isSubsetOf #-}

-- | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf
    :: NEIntSet
    -> NEIntSet
    -> Bool
isProperSubsetOf s0 s1 = S.size (neisIntSet s0) < S.size (neisIntSet s1)
                      && s0 `isSubsetOf` s1
{-# INLINE isProperSubsetOf #-}

-- | /O(n+m)/. Check whether two sets are disjoint (i.e. their intersection
--   is empty).
--
-- > disjoint (fromList (2:|[4,6]))   (fromList (1:|[3]))     == True
-- > disjoint (fromList (2:|[4,6,8])) (fromList (2:|[3,5,7])) == False
-- > disjoint (fromList (1:|[2]))     (fromList (1:|[2,3,4])) == False
disjoint
    :: NEIntSet
    -> NEIntSet
    -> Bool
disjoint n1@(NEIntSet x1 s1) n2@(NEIntSet x2 s2) = case compare x1 x2 of
    -- x1 is not in n2
    LT -> s1 `disjointSet` toSet n2
    -- k1 and k2 are a part of the result
    EQ -> False
    -- k2 is not in n1
    GT -> toSet n1 `disjointSet` s2
{-# INLINE disjoint #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Difference of two sets.
--
-- Returns a potentially empty set ('IntSet') because the first set might be
-- a subset of the second set, and therefore have all of its elements
-- removed.
difference
    :: NEIntSet
    -> NEIntSet
    -> IntSet
difference n1@(NEIntSet x1 s1) n2@(NEIntSet x2 s2) = case compare x1 x2 of
    -- x1 is not in n2, so cannot be deleted
    LT -> insertMinSet x1 $ s1 `S.difference` toSet n2
    -- x2 deletes x1, and only x1
    EQ -> s1 `S.difference` s2
    -- x2 is not in n1, so cannot delete anything, so we can just difference n1 // s2.
    GT -> toSet n1 `S.difference` s2
{-# INLINE difference #-}

-- | Same as 'difference'.
(\\)
    :: NEIntSet
    -> NEIntSet
    -> IntSet
(\\) = difference
{-# INLINE (\\) #-}

-- | /O(m*log(n\/m + 1)), m <= n/. The intersection of two sets.
--
-- Returns a potentially empty set ('IntSet'), because the two sets might have
-- an empty intersection.
--
-- Elements of the result come from the first set, so for example
--
-- > import qualified Data.IntSet.NonEmpty as NES
-- > data AB = A | B deriving Show
-- > instance Ord AB where compare _ _ = EQ
-- > instance Eq AB where _ == _ = True
-- > main = print (NES.singleton A `NES.intersection` NES.singleton B,
-- >               NES.singleton B `NES.intersection` NES.singleton A)
--
-- prints @(fromList (A:|[]),fromList (B:|[]))@.
intersection
    :: NEIntSet
    -> NEIntSet
    -> IntSet
intersection n1@(NEIntSet x1 s1) n2@(NEIntSet x2 s2) = case compare x1 x2 of
    -- x1 is not in n2
    LT -> s1 `S.intersection` toSet n2
    -- x1 and x2 are a part of the result
    EQ -> insertMinSet x1 $ s1 `S.intersection` s2
    -- x2 is not in n1
    GT -> toSet n1 `S.intersection` s2
{-# INLINE intersection #-}

-- | /O(n)/. Filter all elements that satisfy the predicate.
--
-- Returns a potentially empty set ('IntSet') because the predicate might
-- filter out all items in the original non-empty set.
filter
    :: (Key -> Bool)
    -> NEIntSet
    -> IntSet
filter f (NEIntSet x s1)
    | f x       = insertMinSet x . S.filter f $ s1
    | otherwise = S.filter f s1
{-# INLINE filter #-}

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
    :: (Key -> Bool)
    -> NEIntSet
    -> These NEIntSet NEIntSet
partition f n@(NEIntSet x s0) = case (nonEmptySet s1, nonEmptySet s2) of
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
-- containing up to two 'NEIntSet's based on splitting the set into sets
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
    :: Key
    -> NEIntSet
    -> Maybe (These NEIntSet NEIntSet)
split x n@(NEIntSet x0 s0) = case compare x x0 of
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
    :: Key
    -> NEIntSet
    -> (Bool, Maybe (These NEIntSet NEIntSet))
splitMember x n@(NEIntSet x0 s0) = case compare x x0 of
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
    :: NEIntSet
    -> NonEmpty NEIntSet
splitRoot (NEIntSet x s) = singleton x
                     :| mapMaybe nonEmptySet (S.splitRoot s)
{-# INLINE splitRoot #-}

-- | /O(n*log n)/.
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@
map :: (Key -> Key)
    -> NEIntSet
    -> NEIntSet
map f (NEIntSet x0 s) = fromList
                      . (f x0 :|)
                      . S.foldr (\x xs -> f x : xs) []
                      $ s
{-# INLINE map #-}

-- | /O(1)/. The minimal element of a set.  Note that this is total, making
-- 'Data.IntSet.lookupMin' obsolete.  It is constant-time, so has better
-- asymptotics than @Data.IntSet.lookupMin@ and @Data.Map.findMin@ as well.
--
-- > findMin (fromList (5 :| [3])) == 3
findMin :: NEIntSet -> Key
findMin (NEIntSet x _) = x
{-# INLINE findMin #-}

-- | /O(log n)/. The maximal key of a set  Note that this is total,
-- making 'Data.IntSet.lookupMin' obsolete.
--
-- > findMax (fromList (5 :| [3])) == 5
findMax :: NEIntSet -> Key
findMax (NEIntSet x s) = maybe x fst . S.maxView $ s
{-# INLINE findMax #-}

-- | /O(1)/. Delete the minimal element.  Returns a potentially empty set
-- ('IntSet'), because we might delete the final item in a singleton set.  It
-- is constant-time, so has better asymptotics than @Data.IntSet.deleteMin@.
--
-- > deleteMin (fromList (5 :| [3, 7])) == Data.IntSet.fromList [5, 7]
-- > deleteMin (singleton 5) == Data.IntSet.empty
deleteMin :: NEIntSet -> IntSet
deleteMin (NEIntSet _ s) = s
{-# INLINE deleteMin #-}

-- | /O(log n)/. Delete the maximal element.  Returns a potentially empty
-- set ('IntSet'), because we might delete the final item in a singleton set.
--
-- > deleteMax (fromList (5 :| [3, 7])) == Data.IntSet.fromList [3, 5]
-- > deleteMax (singleton 5) == Data.IntSet.empty
deleteMax :: NEIntSet -> IntSet
deleteMax (NEIntSet x s) = case S.maxView s of
    Nothing      -> S.empty
    Just (_, s') -> insertMinSet x s'
{-# INLINE deleteMax #-}

-- | /O(1)/. Delete and find the minimal element.  It is constant-time, so
-- has better asymptotics that @Data.IntSet.minView@ for 'IntSet'.
--
-- Note that unlike @Data.IntSet.deleteFindMin@ for 'IntSet', this cannot ever
-- fail, and so is a total function. However, the result 'IntSet' is
-- potentially empty, since the original set might have contained just
-- a single item.
--
-- > deleteFindMin (fromList (5 :| [3, 10])) == (3, Data.IntSet.fromList [5, 10])
deleteFindMin :: NEIntSet -> (Key, IntSet)
deleteFindMin (NEIntSet x s) = (x, s)
{-# INLINE deleteFindMin #-}

-- | /O(log n)/. Delete and find the minimal element.
--
-- Note that unlike @Data.IntSet.deleteFindMax@ for 'IntSet', this cannot ever
-- fail, and so is a total function. However, the result 'IntSet' is
-- potentially empty, since the original set might have contained just
-- a single item.
--
-- > deleteFindMax (fromList (5 :| [3, 10])) == (10, Data.IntSet.fromList [3, 5])
deleteFindMax :: NEIntSet -> (Key, IntSet)
deleteFindMax (NEIntSet x s) = maybe (x, S.empty) (second (insertMinSet x))
                             . S.maxView
                             $ s
{-# INLINE deleteFindMax #-}

-- | /O(n)/. An alias of 'toAscList'. The elements of a set in ascending
-- order.
elems :: NEIntSet -> NonEmpty Key
elems = toList
{-# INLINE elems #-}

-- | /O(n)/. Convert the set to an ascending non-empty list of elements.
toAscList :: NEIntSet -> NonEmpty Key
toAscList = toList
{-# INLINE toAscList #-}

-- | /O(n)/. Convert the set to a descending non-empty list of elements.
toDescList :: NEIntSet -> NonEmpty Key
toDescList (NEIntSet x s) = S.foldl' (flip (NE.<|)) (x :| []) s
{-# INLINE toDescList #-}

combineEq :: NonEmpty Key -> NonEmpty Key
combineEq (x :| xs) = go x xs
  where
    go z [] = z :| []
    go z (y:ys)
      | z == y    = go z ys
      | otherwise = z NE.<| go y ys
