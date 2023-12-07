{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE EmptyCase       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

-- |
-- Module      : Data.Map.NonEmpty
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- = Non-Empty Finite Maps (lazy interface)
--
-- The @'NEMap' k v@ type represents a non-empty finite map (sometimes
-- called a dictionary) from keys of type @k@ to values of type @v@.
-- An 'NEMap' is strict in its keys but lazy in its values.
--
-- See documentation for 'NEMap' for information on how to convert and
-- manipulate such non-empty maps.
--
-- This module essentially re-imports the API of "Data.Map.Lazy" and its
-- 'Map' type, along with semantics and asymptotics.  In most situations,
-- asymptotics are different only by a constant factor.  In some
-- situations, asmyptotics are even better (constant-time instead of
-- log-time).  All typeclass constraints are identical to their "Data.Map"
-- counterparts.
--
-- Because 'NEMap' is implemented using 'Map', all of the caveats of using
-- 'Map' apply (such as the limitation of the maximum size of maps).
--
-- All functions take non-empty maps as inputs.  In situations where their
-- results can be guarunteed to also be non-empty, they also return
-- non-empty maps.  In situations where their results could potentially be
-- empty, 'Map' is returned instead.
--
-- Some variants of functions (like 'alter'', 'alterF'', 'adjustAt',
-- 'adjustMin', 'adjustMax', 'adjustMinWithKey', 'adjustMaxWithKey') are
-- provided in a way restructured to preserve guaruntees of non-empty maps
-- being returned.
--
-- Some functions (like 'mapEither', 'partition', 'spanAntitone', 'split')
-- have modified return types to account for possible configurations of
-- non-emptiness.
--
-- This module is intended to be imported qualified, to avoid name clashes with
-- "Prelude" and "Data.Map" functions:
--
-- > import qualified Data.Map.NonEmpty as NEM
--
-- At the moment, this package does not provide a variant strict on values
-- for these functions, like /containers/ does.  This is a planned future
-- implementation (PR's are appreciated).  For now, you can simulate
-- a strict interface by manually forcing values before returning results.
module Data.Map.NonEmpty (
  -- * Non-Empty Map type
    NEMap
  -- ** Conversions between empty and non-empty maps
  , pattern IsNonEmpty
  , pattern IsEmpty
  , nonEmptyMap
  , toMap
  , withNonEmpty
  , insertMap
  , insertMapWith
  , insertMapWithKey
  , insertMapMin
  , insertMapMax
  , unsafeFromMap

  -- * Construction
  , singleton
  , fromSet

  -- ** From Unordered Lists
  , fromList
  , fromListWith
  , fromListWithKey

  -- ** From Ascending Lists
  , fromAscList
  , fromAscListWith
  , fromAscListWithKey
  , fromDistinctAscList

  -- ** From Descending Lists
  , fromDescList
  , fromDescListWith
  , fromDescListWithKey
  , fromDistinctDescList

  -- * Insertion
  , insert
  , insertWith
  , insertWithKey
  , insertLookupWithKey

  -- * Deletion\/Update
  , delete
  , adjust
  , adjustWithKey
  , update
  , updateWithKey
  , updateLookupWithKey
  , alter
  , alterF
  , alter'
  , alterF'

  -- * Query
  -- ** Lookup
  , lookup
  , (!?)
  , (!)
  , findWithDefault
  , member
  , notMember
  , lookupLT
  , lookupGT
  , lookupLE
  , lookupGE
  , absurdNEMap

  -- ** Size
  , size

  -- * Combine

  -- ** Union
  , union
  , unionWith
  , unionWithKey
  , unions
  , unionsWith

  -- ** Difference
  , difference
  , (\\)
  , differenceWith
  , differenceWithKey

  -- ** Intersection
  , intersection
  , intersectionWith
  , intersectionWithKey

  -- -- ** Unsafe general combining function
  -- , mergeWithKey

  -- * Traversal
  -- ** Map
  , map
  , mapWithKey
  , traverseWithKey1
  , traverseWithKey
  , traverseMaybeWithKey1
  , traverseMaybeWithKey
  , mapAccum
  , mapAccumWithKey
  , mapAccumRWithKey
  , mapKeys
  , mapKeysWith
  , mapKeysMonotonic

  -- * Folds
  , foldr
  , foldl
  , foldr1
  , foldl1
  , foldrWithKey
  , foldlWithKey
  , foldMapWithKey

  -- ** Strict folds
  , foldr'
  , foldr1'
  , foldl'
  , foldl1'
  , foldrWithKey'
  , foldlWithKey'

  -- * Conversion
  , elems
  , keys
  , assocs
  , keysSet

  -- ** Lists
  , toList

  -- ** Ordered lists
  , toAscList
  , toDescList

  -- * Filter
  , filter
  , filterWithKey
  , restrictKeys
  , withoutKeys
  , partition
  , partitionWithKey
  , takeWhileAntitone
  , dropWhileAntitone
  , spanAntitone

  , mapMaybe
  , mapMaybeWithKey
  , mapEither
  , mapEitherWithKey

  , split
  , splitLookup
  , splitRoot

  -- * Submap
  , isSubmapOf, isSubmapOfBy
  , isProperSubmapOf, isProperSubmapOfBy

  -- * Indexed
  , lookupIndex
  , findIndex
  , elemAt
  , updateAt
  , adjustAt
  , deleteAt
  , take
  , drop
  , splitAt

  -- * Min\/Max
  , findMin
  , findMax
  , deleteMin
  , deleteMax
  , deleteFindMin
  , deleteFindMax
  , updateMin
  , updateMax
  , adjustMin
  , adjustMax
  , updateMinWithKey
  , updateMaxWithKey
  , adjustMinWithKey
  , adjustMaxWithKey
  , minView
  , maxView

  -- * Debugging
  , valid
  ) where

import           Control.Applicative
import           Data.Bifunctor
import           Data.Function
import           Data.Functor.Apply
import           Data.Functor.Identity
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Map                   (Map)
import           Data.Map.NonEmpty.Internal
import           Data.Maybe hiding          (mapMaybe)
import           Data.Semigroup.Foldable    (Foldable1)
import           Data.Set                   (Set)
import           Data.Set.NonEmpty.Internal (NESet(..))
import           Data.These
import           Data.Void
import           Prelude hiding             (Foldable(..), lookup, filter, map, take, drop, splitAt)
import qualified Data.Foldable              as F
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Maybe                 as Maybe
import qualified Data.Semigroup.Foldable    as F1
import qualified Data.Set                   as S

-- | /O(1)/ match, /O(log n)/ usage of contents. The 'IsNonEmpty' and
-- 'IsEmpty' patterns allow you to treat a 'Map' as if it were either
-- a @'IsNonEmpty' n@ (where @n@ is a 'NEMap') or an 'IsEmpty'.
--
-- For example, you can pattern match on a 'Map':
--
-- @
-- myFunc :: 'Map' K X -> Y
-- myFunc ('IsNonEmpty' n) =  -- here, the user provided a non-empty map, and @n@ is the 'NEMap'
-- myFunc 'IsEmpty'        =  -- here, the user provided an empty map.
-- @
--
-- Matching on @'IsNonEmpty' n@ means that the original 'Map' was /not/
-- empty, and you have a verified-non-empty 'NEMap' @n@ to use.
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
-- a 'NEMap' back into a 'Map', obscuring its non-emptiness (see 'toMap').
pattern IsNonEmpty :: NEMap k a -> Map k a
pattern IsNonEmpty n <- (nonEmptyMap->Just n)
  where
    IsNonEmpty n = toMap n

-- | /O(1)/. The 'IsNonEmpty' and 'IsEmpty' patterns allow you to treat
-- a 'Map' as if it were either a @'IsNonEmpty' n@ (where @n@ is
-- a 'NEMap') or an 'IsEmpty'.
--
-- Matching on 'IsEmpty' means that the original 'Map' was empty.
--
-- A case statement handling both 'IsNonEmpty' and 'IsEmpty' provides
-- complete coverage.
--
-- This is a bidirectional pattern, so you can use 'IsEmpty' as an
-- expression, and it will be interpreted as 'Data.Map.empty'.
--
-- See 'IsNonEmpty' for more information.
pattern IsEmpty :: Map k a
pattern IsEmpty <- (M.null->True)
  where
    IsEmpty = M.empty

{-# COMPLETE IsNonEmpty, IsEmpty #-}

-- | /O(log n)/. Unsafe version of 'nonEmptyMap'.  Coerces a 'Map' into an
-- 'NEMap', but is undefined (throws a runtime exception when evaluation is
-- attempted) for an empty 'Map'.
unsafeFromMap
    :: Map k a
    -> NEMap k a
unsafeFromMap = withNonEmpty e id
  where
    e = errorWithoutStackTrace "NEMap.unsafeFromMap: empty map"
{-# INLINE unsafeFromMap #-}

-- | /O(n)/. Build a non-empty map from a non-empty set of keys and
-- a function which for each key computes its value.
--
-- > fromSet (\k -> replicate k 'a') (Data.Set.NonEmpty.fromList (3 :| [5])) == fromList ((5,"aaaaa") :| [(3,"aaa")])
fromSet
    :: (k -> a)
    -> NESet k
    -> NEMap k a
fromSet f (NESet k ks) = NEMap k (f k) (M.fromSet f ks)
{-# INLINE fromSet #-}

-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
--
-- An example of using @lookup@:
--
-- > import Prelude hiding (lookup)
-- > import Data.Map.NonEmpty
-- >
-- > employeeDept = fromList (("John","Sales") :| [("Bob","IT")])
-- > deptCountry = fromList (("IT","USA") :| [("Sales","France")])
-- > countryCurrency = fromList (("USA", "Dollar") :| [("France", "Euro")])
-- >
-- > employeeCurrency :: String -> Maybe String
-- > employeeCurrency name = do
-- >     dept <- lookup name employeeDept
-- >     country <- lookup dept deptCountry
-- >     lookup country countryCurrency
-- >
-- > main = do
-- >     putStrLn $ "John's currency: " ++ (show (employeeCurrency "John"))
-- >     putStrLn $ "Pete's currency: " ++ (show (employeeCurrency "Pete"))
--
-- The output of this program:
--
-- >   John's currency: Just "Euro"
-- >   Pete's currency: Nothing
lookup
    :: Ord k
    => k
    -> NEMap k a
    -> Maybe a
lookup k (NEMap k0 v m) = case compare k k0 of
    LT -> Nothing
    EQ -> Just v
    GT -> M.lookup k m
{-# INLINE lookup #-}

-- | /O(log n)/. Find the value at a key. Returns 'Nothing' when the
-- element can not be found.
--
-- prop> fromList ((5, 'a') :| [(3, 'b')]) !? 1 == Nothing
-- prop> fromList ((5, 'a') :| [(3, 'b')]) !? 5 == Just 'a'
(!?) :: Ord k => NEMap k a -> k -> Maybe a
(!?) = flip lookup
{-# INLINE (!?) #-}

-- | /O(log n)/. Find the value at a key. Calls 'error' when the element
-- can not be found.
--
-- > fromList ((5,'a') :| [(3,'b')]) ! 1    Error: element not in the map
-- > fromList ((5,'a') :| [(3,'b')]) ! 5 == 'a'
(!) :: Ord k => NEMap k a -> k -> a
(!) m k = fromMaybe e $ m !? k
  where
    e = error "NEMap.!: given key is not an element in the map"
{-# INLINE (!) #-}

infixl 9 !?
infixl 9 !

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
--
-- > findWithDefault 'x' 1 (fromList ((5,'a') :| [(3,'b')])) == 'x'
-- > findWithDefault 'x' 5 (fromList ((5,'a') :| [(3,'b')])) == 'a'
findWithDefault
    :: Ord k
    => a
    -> k
    -> NEMap k a
    -> a
findWithDefault def k (NEMap k0 v m) = case compare k k0 of
    LT -> def
    EQ -> v
    GT -> M.findWithDefault def k m
{-# INLINE findWithDefault #-}

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
--
-- > member 5 (fromList ((5,'a') :| [(3,'b')])) == True
-- > member 1 (fromList ((5,'a') :| [(3,'b')])) == False
member :: Ord k => k -> NEMap k a -> Bool
member k (NEMap k0 _ m) = case compare k k0 of
    LT -> False
    EQ -> True
    GT -> M.member k m
{-# INLINE member #-}

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
--
-- > notMember 5 (fromList ((5,'a') :| [(3,'b')])) == False
-- > notMember 1 (fromList ((5,'a') :| [(3,'b')])) == True
notMember :: Ord k => k -> NEMap k a -> Bool
notMember k (NEMap k0 _ m) = case compare k k0 of
    LT -> True
    EQ -> False
    GT -> M.notMember k m
{-# INLINE notMember #-}

-- | /O(log n)/. Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList ((3,'a') :| [(5,'b')])) == Nothing
-- > lookupLT 4 (fromList ((3,'a') :| [(5,'b')])) == Just (3, 'a')
lookupLT :: Ord k => k -> NEMap k a -> Maybe (k, a)
lookupLT k (NEMap k0 v m) = case compare k k0 of
    LT -> Nothing
    EQ -> Nothing
    GT -> M.lookupLT k m <|> Just (k0, v)
{-# INLINE lookupLT #-}

-- | /O(log n)/. Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList ((3,'a') :| [(5,'b')])) == Just (5, 'b')
-- > lookupGT 5 (fromList ((3,'a') :| [(5,'b')])) == Nothing
lookupGT :: Ord k => k -> NEMap k a -> Maybe (k, a)
lookupGT k (NEMap k0 v m) = case compare k k0 of
    LT -> Just (k0, v)
    EQ -> M.lookupMin m
    GT -> M.lookupGT k m
{-# INLINE lookupGT #-}

-- | /O(log n)/. Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList ((3,'a') :| [(5,'b')])) == Nothing
-- > lookupLE 4 (fromList ((3,'a') :| [(5,'b')])) == Just (3, 'a')
-- > lookupLE 5 (fromList ((3,'a') :| [(5,'b')])) == Just (5, 'b')
lookupLE :: Ord k => k -> NEMap k a -> Maybe (k, a)
lookupLE k (NEMap k0 v m) = case compare k k0 of
    LT -> Nothing
    EQ -> Just (k0, v)
    GT -> M.lookupLE k m <|> Just (k0, v)
{-# INLINE lookupLE #-}

-- | /O(log n)/. Find smallest key greater or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupGE 3 (fromList ((3,'a') :| [(5,'b')])) == Just (3, 'a')
-- > lookupGE 4 (fromList ((3,'a') :| [(5,'b')])) == Just (5, 'b')
-- > lookupGE 6 (fromList ((3,'a') :| [(5,'b')])) == Nothing
lookupGE :: Ord k => k -> NEMap k a -> Maybe (k, a)
lookupGE k (NEMap k0 v m) = case compare k k0 of
    LT -> Just (k0, v)
    EQ -> Just (k0, v)
    GT -> M.lookupGE k m
{-# INLINE lookupGE #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Union with a combining function.
--
-- > unionWith (++) (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == fromList ((3, "b") :| [(5, "aA"), (7, "C")])
unionWith
    :: Ord k
    => (a -> a -> a)
    -> NEMap k a
    -> NEMap k a
    -> NEMap k a
unionWith f n1@(NEMap k1 v1 m1) n2@(NEMap k2 v2 m2) = case compare k1 k2 of
    LT -> NEMap k1 v1        . M.unionWith f m1 . toMap $ n2
    EQ -> NEMap k1 (f v1 v2) . M.unionWith f m1         $ m2
    GT -> NEMap k2 v2        . M.unionWith f (toMap n1) $ m2
{-# INLINE unionWith #-}

-- | /O(m*log(n\/m + 1)), m <= n/.
-- Union with a combining function, given the matching key.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == fromList ((3, "b") :| [(5, "5:a|A"), (7, "C")])
unionWithKey
    :: Ord k
    => (k -> a -> a -> a)
    -> NEMap k a
    -> NEMap k a
    -> NEMap k a
unionWithKey f n1@(NEMap k1 v1 m1) n2@(NEMap k2 v2 m2) = case compare k1 k2 of
    LT -> NEMap k1 v1           . M.unionWithKey f m1 . toMap $ n2
    EQ -> NEMap k1 (f k1 v1 v2) . M.unionWithKey f m1         $ m2
    GT -> NEMap k2 v2           . M.unionWithKey f (toMap n1) $ m2
{-# INLINE unionWithKey #-}

-- | The union of a non-empty list of maps, with a combining operation:
--   (@'unionsWith' f == 'Data.Foldable.foldl1' ('unionWith' f)@).
--
-- > unionsWith (++) (fromList ((5, "a") :| [(3, "b")]) :| [fromList ((5, "A") :| [(7, "C")]), fromList ((5, "A3") :| [(3, "B3")])])
-- >     == fromList ((3, "bB3") :| [(5, "aAA3"), (7, "C")])
unionsWith
    :: (Foldable1 f, Ord k)
    => (a -> a -> a)
    -> f (NEMap k a)
    -> NEMap k a
unionsWith f (F1.toNonEmpty->(m :| ms)) = F.foldl' (unionWith f) m ms
{-# INLINE unionsWith #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Difference of two maps.
-- Return elements of the first map not existing in the second map.
--
-- Returns a potentially empty map ('Map'), in case the first map is
-- a subset of the second map.
--
-- > difference (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == Data.Map.singleton 3 "b"
difference
    :: Ord k
    => NEMap k a
    -> NEMap k b
    -> Map k a
difference n1@(NEMap k1 v1 m1) n2@(NEMap k2 _ m2) = case compare k1 k2 of
    -- k1 is not in n2, so cannot be deleted
    LT -> insertMinMap k1 v1 $ m1 `M.difference` toMap n2
    -- k2 deletes k1, and only k1
    EQ -> m1 `M.difference` m2
    -- k2 is not in n1, so cannot delete anything, so we can just difference n1 // m2.
    GT -> toMap n1 `M.difference` m2
{-# INLINE difference #-}

-- | Same as 'difference'.
(\\)
    :: Ord k
    => NEMap k a
    -> NEMap k b
    -> Map k a
(\\) = difference
{-# INLINE (\\) #-}

-- | /O(n+m)/. Difference with a combining function.
-- When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- Returns a potentially empty map ('Map'), in case the first map is
-- a subset of the second map and the function returns 'Nothing' for every
-- pair.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(3, "B"), (7, "C")]))
-- >     == Data.Map.singleton 3 "b:B"
differenceWith
    :: Ord k
    => (a -> b -> Maybe a)
    -> NEMap k a
    -> NEMap k b
    -> Map k a
differenceWith f = differenceWithKey (const f)
{-# INLINE differenceWith #-}

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- Returns a potentially empty map ('Map'), in case the first map is
-- a subset of the second map and the function returns 'Nothing' for every
-- pair.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(3, "B"), (10, "C")]))
-- >     == Data.Map.singleton 3 "3:b|B"
differenceWithKey
    :: Ord k
    => (k -> a -> b -> Maybe a)
    -> NEMap k a
    -> NEMap k b
    -> Map k a
differenceWithKey f n1@(NEMap k1 v1 m1) n2@(NEMap k2 v2 m2) = case compare k1 k2 of
    -- k1 is not in n2, so cannot be deleted
    LT -> insertMinMap k1 v1 $ M.differenceWithKey f m1 (toMap n2)
    -- k2 deletes k1, and only k1
    EQ -> ($ M.differenceWithKey f m1 m2) . maybe id (insertMinMap k1) $ f k1 v1 v2
    -- k2 is not in n1, so cannot delete anything, so we can just difference n1 // m2.
    GT -> M.differenceWithKey f (toMap n1) m2
{-# INLINE differenceWithKey #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Intersection of two maps.
-- Return data in the first map for the keys existing in both maps.
-- (@'intersection' m1 m2 == 'intersectionWith' 'const' m1 m2@).
--
-- Returns a potentially empty map ('Map'), in case the two maps share no
-- keys in common.
--
-- > intersection (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == Data.Map.singleton 5 "a"
intersection
    :: Ord k
    => NEMap k a
    -> NEMap k b
    -> Map k a
intersection n1@(NEMap k1 v1 m1) n2@(NEMap k2 _ m2) = case compare k1 k2 of
    -- k1 is not in n2
    LT -> m1 `M.intersection` toMap n2
    -- k1 and k2 are a part of the result
    EQ -> insertMinMap k1 v1 $ m1 `M.intersection` m2
    -- k2 is not in n1
    GT -> toMap n1 `M.intersection` m2
{-# INLINE intersection #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Intersection with a combining function.
--
-- Returns a potentially empty map ('Map'), in case the two maps share no
-- keys in common.
--
-- > intersectionWith (++) (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == Data.Map.singleton 5 "aA"
intersectionWith
    :: Ord k
    => (a -> b -> c)
    -> NEMap k a
    -> NEMap k b
    -> Map k c
intersectionWith f = intersectionWithKey (const f)
{-# INLINE intersectionWith #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Intersection with a combining function.
--
-- Returns a potentially empty map ('Map'), in case the two maps share no
-- keys in common.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == Data.Map.singleton 5 "5:a|A"
intersectionWithKey
    :: Ord k
    => (k -> a -> b -> c)
    -> NEMap k a
    -> NEMap k b
    -> Map k c
intersectionWithKey f n1@(NEMap k1 v1 m1) n2@(NEMap k2 v2 m2) = case compare k1 k2 of
    -- k1 is not in n2
    LT -> M.intersectionWithKey f m1 (toMap n2)
    -- k1 and k2 are a part of the result
    EQ -> insertMinMap k1 (f k1 v1 v2) $ M.intersectionWithKey f m1 m2
    -- k2 is not in n1
    GT -> M.intersectionWithKey f (toMap n1) m2
{-# INLINE intersectionWithKey #-}

-- | /O(n)/. A strict version of 'foldr1'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr1' :: (a -> a -> a) -> NEMap k a -> a
foldr1' f (NEMap _ v m) = case M.maxView m of
    Nothing      -> v
    Just (y, m') -> let !z = M.foldr' f y m' in v `f` z
{-# INLINE foldr1' #-}

-- | /O(n)/. A strict version of 'foldl1'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl1' :: (a -> a -> a) -> NEMap k a -> a
foldl1' f (NEMap _ v m) = M.foldl' f v m
{-# INLINE foldl1' #-}

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keysList map = foldrWithKey (\k x ks -> k:ks) [] map
foldrWithKey :: (k -> a -> b -> b) -> b -> NEMap k a -> b
foldrWithKey f z (NEMap k v m) = f k v . M.foldrWithKey f z $ m
{-# INLINE foldrWithKey #-}

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: (k -> a -> b -> b) -> b -> NEMap k a -> b
foldrWithKey' f z (NEMap k v m) = f k v y
  where
    !y = M.foldrWithKey f z m
{-# INLINE foldrWithKey' #-}

-- | /O(n)/. Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- For example,
--
-- > keysList = reverse . foldlWithKey (\ks k x -> k:ks) []
foldlWithKey :: (a -> k -> b -> a) -> a -> NEMap k b -> a
foldlWithKey f z (NEMap k v m) = M.foldlWithKey f (f z k v) m
{-# INLINE foldlWithKey #-}

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (a -> k -> b -> a) -> a -> NEMap k b -> a
foldlWithKey' f z (NEMap k v m) = M.foldlWithKey' f x m
  where
    !x = f z k v
{-# INLINE foldlWithKey' #-}

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList ((5,"a") :| [(3,"b")])) == (3 :| [5])
keys :: NEMap k a -> NonEmpty k
keys (NEMap k _ m) = k :| M.keys m
{-# INLINE keys #-}

-- | /O(n)/. An alias for 'toAscList'. Return all key\/value pairs in the map
-- in ascending key order.
--
-- > assocs (fromList ((5,"a") :| [(3,"b")])) == ((3,"b") :| [(5,"a")])
assocs :: NEMap k a -> NonEmpty (k, a)
assocs = toList
{-# INLINE assocs #-}

-- | /O(n)/. The non-empty set of all keys of the map.
--
-- > keysSet (fromList ((5,"a") :| [(3,"b")])) == Data.Set.NonEmpty.fromList (3 :| [5])
keysSet :: NEMap k a -> NESet k
keysSet (NEMap k _ m) = NESet k (M.keysSet m)
{-# INLINE keysSet #-}

-- | /O(n)/. Map a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "3:b") :| [(5, "5:a")])
mapWithKey :: (k -> a -> b) -> NEMap k a -> NEMap k b
mapWithKey f (NEMap k v m) = NEMap k (f k v) (M.mapWithKey f m)
{-# NOINLINE [1] mapWithKey #-}
{-# RULES
"mapWithKey/mapWithKey" forall f g xs . mapWithKey f (mapWithKey g xs) =
  mapWithKey (\k a -> f k (g k a)) xs
"mapWithKey/map" forall f g xs . mapWithKey f (map g xs) =
  mapWithKey (\k a -> f k (g a)) xs
"map/mapWithKey" forall f g xs . map f (mapWithKey g xs) =
  mapWithKey (\k a -> f (g k a)) xs
 #-}

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys are
-- in ascending order.
--
-- > toAscList (fromList ((5,"a") :| [(3,"b")])) == ((3,"b") :| [(5,"a")])
toAscList :: NEMap k a -> NonEmpty (k, a)
toAscList = toList
{-# INLINE toAscList #-}

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys
-- are in descending order.
--
-- > toDescList (fromList ((5,"a") :| [(3,"b")])) == ((5,"a") :| [(3,"b")])
toDescList :: NEMap k a -> NonEmpty (k, a)
toDescList (NEMap k0 v0 m) = M.foldlWithKey' go ((k0, v0) :| []) m
  where
    go xs k v = (k, v) NE.<| xs
{-# INLINE toDescList #-}

-- | /O(log n)/. Convert a 'Map' into an 'NEMap' by adding a key-value
-- pair.  Because of this, we know that the map must have at least one
-- element, and so therefore cannot be empty. If key is already present,
-- will overwrite the original value.
--
-- See 'insertMapMin' for a version that is constant-time if the new key is
-- /strictly smaller than/ all keys in the original map.
--
-- > insertMap 4 "c" (Data.Map.fromList [(5,"a"), (3,"b")]) == fromList ((3,"b") :| [(4,"c"), (5,"a")])
-- > insertMap 4 "c" Data.Map.empty == singleton 4 "c"
insertMap :: Ord k => k -> a -> Map k a -> NEMap k a
insertMap k v = withNonEmpty (singleton k v) (insert k v)
{-# INLINE insertMap #-}

-- | /O(log n)/. Convert a 'Map' into an 'NEMap' by adding a key-value
-- pair.  Because of this, we know that the map must have at least one
-- element, and so therefore cannot be empty. Uses a combining function
-- with the new value as the first argument if the key is already present.
--
-- > insertMapWith (++) 4 "c" (Data.Map.fromList [(5,"a"), (3,"b")]) == fromList ((3,"b") :| [(4,"c"), (5,"a")])
-- > insertMapWith (++) 5 "c" (Data.Map.fromList [(5,"a"), (3,"b")]) == fromList ((3,"b") :| [(5,"ca")])
insertMapWith
    :: Ord k
    => (a -> a -> a)
    -> k
    -> a
    -> Map k a
    -> NEMap k a
insertMapWith f k v = withNonEmpty (singleton k v) (insertWith f k v)
{-# INLINE insertMapWith #-}

-- | /O(log n)/. Convert a 'Map' into an 'NEMap' by adding a key-value
-- pair.  Because of this, we know that the map must have at least one
-- element, and so therefore cannot be empty. Uses a combining function
-- with the key and new value as the first and second arguments if the key
-- is already present.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (Data.Map.fromList [(5,"a"), (3,"b")]) == fromList ((3, "b") :| [(5, "5:xxx|a")])
-- > insertWithKey f 7 "xxx" (Data.Map.fromList [(5,"a"), (3,"b")]) == fromList ((3, "b") :| [(5, "a"), (7, "xxx")])
-- > insertWithKey f 5 "xxx" Data.Map.empty                         == singleton 5 "xxx"
insertMapWithKey
    :: Ord k
    => (k -> a -> a -> a)
    -> k
    -> a
    -> Map k a
    -> NEMap k a
insertMapWithKey f k v = withNonEmpty (singleton k v) (insertWithKey f k v)
{-# INLINE insertMapWithKey #-}

-- | /O(1)/ Convert a 'Map' into an 'NEMap' by adding a key-value pair
-- where the key is /strictly less than/ all keys in the input map.  The
-- keys in the original map must all be /strictly greater than/ the new
-- key.  /The precondition is not checked./
--
-- > insertMapMin 2 "c" (Data.Map.fromList [(5,"a"), (3,"b")]) == fromList ((2,"c") :| [(3,"b"), (5,"a")])
-- > valid (insertMapMin 2 "c" (Data.Map.fromList [(5,"a"), (3,"b")])) == True
-- > valid (insertMapMin 7 "c" (Data.Map.fromList [(5,"a"), (3,"b")])) == False
-- > valid (insertMapMin 3 "c" (Data.Map.fromList [(5,"a"), (3,"b")])) == False
insertMapMin
    :: k
    -> a
    -> Map k a
    -> NEMap k a
insertMapMin = NEMap
{-# INLINE insertMapMin #-}

-- | /O(log n)/ Convert a 'Map' into an 'NEMap' by adding a key-value pair
-- where the key is /strictly greater than/ all keys in the input map.  The
-- keys in the original map must all be /strictly less than/ the new
-- key.  /The precondition is not checked./
--
-- While this has the same asymptotics as 'insertMap', it saves a constant
-- factor for key comparison (so may be helpful if comparison is expensive)
-- and also does not require an 'Ord' instance for the key type.
--
-- > insertMap 7 "c" (Data.Map.fromList [(5,"a"), (3,"b")]) == fromList ((3,"b") :| [(5,"a"), (7,"c")])
-- > valid (insertMap 7 "c" (Data.Map.fromList [(5,"a"), (3,"b")])) == True
-- > valid (insertMap 2 "c" (Data.Map.fromList [(5,"a"), (3,"b")])) == False
-- > valid (insertMap 5 "c" (Data.Map.fromList [(5,"a"), (3,"b")])) == False
insertMapMax
    :: k
    -> a
    -> Map k a
    -> NEMap k a
insertMapMax k v = withNonEmpty (singleton k v) go
  where
    go (NEMap k0 v0 m0) = NEMap k0 v0 . insertMaxMap k v $ m0
{-# INLINE insertMapMax #-}


-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- See 'insertMap' for a version where the first argument is a 'Map'.
--
-- > insert 5 'x' (fromList ((5,'a') :| [(3,'b')])) == fromList ((3, 'b') :| [(5, 'x')])
-- > insert 7 'x' (fromList ((5,'a') :| [(3,'b')])) == fromList ((3, 'b') :| [(5, 'a'), (7, 'x')])
insert
    :: Ord k
    => k
    -> a
    -> NEMap k a
    -> NEMap k a
insert k v n@(NEMap k0 v0 m) = case compare k k0 of
    LT -> NEMap k  v  . toMap        $ n
    EQ -> NEMap k  v  m
    GT -> NEMap k0 v0 . M.insert k v $ m
{-# INLINE insert #-}

-- | /O(log n)/. Insert with a function, combining key, new value and old
-- value. @'insertWithKey' f key value mp@ will insert the pair (key,
-- value) into @mp@ if key does not exist in the map. If the key does
-- exist, the function will insert the pair @(key,f key new_value
-- old_value)@. Note that the key passed to f is the same key passed to
-- 'insertWithKey'.
--
-- See 'insertMapWithKey' for a version where the first argument is a 'Map'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "5:xxx|a")])
-- > insertWithKey f 7 "xxx" (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "a"), (7, "xxx")])
insertWithKey
    :: Ord k
    => (k -> a -> a -> a)
    -> k
    -> a
    -> NEMap k a
    -> NEMap k a
insertWithKey f k v n@(NEMap k0 v0 m) = case compare k k0 of
    LT -> NEMap k  v          . toMap               $ n
    EQ -> NEMap k  (f k v v0) m
    GT -> NEMap k0 v0         $ M.insertWithKey f k v m
{-# INLINE insertWithKey #-}

-- | /O(log n)/. Combines insert operation with old value retrieval. The
-- expression (@'insertLookupWithKey' f k x map@) is a pair where the first
-- element is equal to (@'lookup' k map@) and the second element equal to
-- (@'insertWithKey' f k x map@).
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertLookupWithKey f 5 "xxx" (fromList ((5,"a") :| [(3,"b")])) == (Just "a", fromList ((3, "b") :| [(5, "5:xxx|a")]))
-- > insertLookupWithKey f 7 "xxx" (fromList ((5,"a") :| [(3,"b")])) == (Nothing,  fromList ((3, "b") :| [(5, "a"), (7, "xxx")]))
--
-- This is how to define @insertLookup@ using @insertLookupWithKey@:
--
-- > let insertLookup kx x t = insertLookupWithKey (\_ a _ -> a) kx x t
-- > insertLookup 5 "x" (fromList ((5,"a") :| [(3,"b")])) == (Just "a", fromList ((3, "b") :| [(5, "x")]))
-- > insertLookup 7 "x" (fromList ((5,"a") :| [(3,"b")])) == (Nothing,  fromList ((3, "b") :| [(5, "a"), (7, "x")]))
insertLookupWithKey
    :: Ord k
    => (k -> a -> a -> a)
    -> k
    -> a
    -> NEMap k a
    -> (Maybe a, NEMap k a)
insertLookupWithKey f k v n@(NEMap k0 v0 m) = case compare k k0 of
    LT -> (Nothing, NEMap k  v . toMap $ n )
    EQ -> (Just v , NEMap k  (f k v v0)  m )
    GT -> NEMap k0 v0 <$> M.insertLookupWithKey f k v m
{-# INLINE insertLookupWithKey #-}

-- | /O(n*log n)/. Build a map from a non-empty list of key\/value pairs
-- with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) ((5,"a") :| [(5,"b"), (3,"b"), (3,"a"), (5,"a")]) == fromList ((3, "ab") :| [(5, "aba")])
fromListWith
    :: Ord k
    => (a -> a -> a)
    -> NonEmpty (k, a)
    -> NEMap k a
fromListWith f = fromListWithKey (const f)
{-# INLINE fromListWith #-}

-- | /O(n*log n)/. Build a map from a non-empty list of key\/value pairs
-- with a combining function. See also 'fromAscListWithKey'.
--
-- > let f k a1 a2 = (show k) ++ a1 ++ a2
-- > fromListWithKey f ((5,"a") :| [(5,"b"), (3,"b"), (3,"a"), (5,"a")]) == fromList ((3, "3ab") :| [(5, "5a5ba")])
fromListWithKey
    :: Ord k
    => (k -> a -> a -> a)
    -> NonEmpty (k, a)
    -> NEMap k a
fromListWithKey f ((k0, v0) :| xs) = F.foldl' go (singleton k0 v0) xs
  where
    go m (k, v) = insertWithKey f k v m
    {-# INLINE go #-}
{-# INLINE fromListWithKey #-}

-- | /O(n)/. Build a map from an ascending non-empty list in linear time.
-- /The precondition (input list is ascending) is not checked./
--
-- > fromAscList ((3,"b") :| [(5,"a")])          == fromList ((3, "b") :| [(5, "a")])
-- > fromAscList ((3,"b") :| [(5,"a"), (5,"b")]) == fromList ((3, "b") :| [(5, "b")])
-- > valid (fromAscList ((3,"b") :| [(5,"a"), (5,"b")])) == True
-- > valid (fromAscList ((5,"a") :| [(3,"b"), (5,"b")])) == False
fromAscList
    :: Eq k
    => NonEmpty (k, a)
    -> NEMap k a
fromAscList = fromDistinctAscList . combineEq
{-# INLINE fromAscList #-}

-- | /O(n)/. Build a map from an ascending non-empty list in linear time
-- with a combining function for equal keys. /The precondition (input list
-- is ascending) is not checked./
--
-- > fromAscListWith (++) ((3,"b") :| [(5,"a"), (5,"b")]) == fromList ((3, "b") :| [(5, "ba")])
-- > valid (fromAscListWith (++) ((3,"b") :| [(5,"a"), (5,"b"))]) == True
-- > valid (fromAscListWith (++) ((5,"a") :| [(3,"b"), (5,"b"))]) == False
fromAscListWith
    :: Eq k
    => (a -> a -> a)
    -> NonEmpty (k, a)
    -> NEMap k a
fromAscListWith f = fromAscListWithKey (const f)
{-# INLINE fromAscListWith #-}

-- | /O(n)/. Build a map from an ascending non-empty list in linear time
-- with a combining function for equal keys. /The precondition (input list
-- is ascending) is not checked./
--
-- > let f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2
-- > fromAscListWithKey f ((3,"b") :| [(5,"a"), (5,"b"), (5,"b")]) == fromList ((3, "b") :| [(5, "5:b5:ba")])
-- > valid (fromAscListWithKey f ((3,"b") :| [(5,"a"), (5,"b"), (5,"b")])) == True
-- > valid (fromAscListWithKey f ((5,"a") :| [(3,"b"), (5,"b"), (5,"b")])) == False
fromAscListWithKey
    :: Eq k
    => (k -> a -> a -> a)
    -> NonEmpty (k, a)
    -> NEMap k a
fromAscListWithKey f = fromDistinctAscList . combineEqWith f
{-# INLINE fromAscListWithKey #-}

-- | /O(n)/. Build a map from an ascending non-empty list of distinct
-- elements in linear time. /The precondition is not checked./
--
-- > fromDistinctAscList ((3,"b") :| [(5,"a")]) == fromList ((3, "b") :| [(5, "a")])
-- > valid (fromDistinctAscList ((3,"b") :| [(5,"a")]))          == True
-- > valid (fromDistinctAscList ((3,"b") :| [(5,"a"), (5,"b")])) == False
fromDistinctAscList :: NonEmpty (k, a) -> NEMap k a
fromDistinctAscList ((k, v) :| xs) = insertMapMin k v
                                   . M.fromDistinctAscList
                                   $ xs
{-# INLINE fromDistinctAscList #-}

-- | /O(n)/. Build a map from a descending non-empty list in linear time.
-- /The precondition (input list is descending) is not checked./
--
-- > fromDescList ((5,"a") :| [(3,"b")])          == fromList ((3, "b") :| [(5, "a")])
-- > fromDescList ((5,"a") :| [(5,"b"), (3,"b")]) == fromList ((3, "b") :| [(5, "b")])
-- > valid (fromDescList ((5,"a") :| [(5,"b"), (3,"b")])) == True
-- > valid (fromDescList ((5,"a") :| [(3,"b"), (5,"b")])) == False
fromDescList
    :: Eq k
    => NonEmpty (k, a)
    -> NEMap k a
fromDescList = fromDistinctDescList . combineEq
{-# INLINE fromDescList #-}

-- | /O(n)/. Build a map from a descending non-empty list in linear time
-- with a combining function for equal keys. /The precondition (input list
-- is descending) is not checked./
--
-- > fromDescListWith (++) ((5,"a") :| [(5,"b"), (3,"b")]) == fromList ((3, "b") :| [(5, "ba")])
-- > valid (fromDescListWith (++) ((5,"a") :| [(5,"b"), (3,"b")])) == True
-- > valid (fromDescListWith (++) ((5,"a") :| [(3,"b"), (5,"b")])) == False
fromDescListWith
    :: Eq k
    => (a -> a -> a)
    -> NonEmpty (k, a)
    -> NEMap k a
fromDescListWith f = fromDescListWithKey (const f)
{-# INLINE fromDescListWith #-}

-- | /O(n)/. Build a map from a descending non-empty list in linear time
-- with a combining function for equal keys. /The precondition (input list
-- is descending) is not checked./
--
-- > let f k a1 a2 = (show k) ++ ":" ++ a1 ++ a2
-- > fromDescListWithKey f ((5,"a") :| [(5,"b"), (5,"b"), (3,"b")]) == fromList ((3, "b") :| [(5, "5:b5:ba")])
-- > valid (fromDescListWithKey f ((5,"a") :| [(5,"b"), (5,"b"), (3,"b")])) == True
-- > valid (fromDescListWithKey f ((5,"a") :| [(3,"b"), (5,"b"), (5,"b")])) == False
fromDescListWithKey
    :: Eq k
    => (k -> a -> a -> a)
    -> NonEmpty (k, a)
    -> NEMap k a
fromDescListWithKey f = fromDistinctDescList . combineEqWith f
{-# INLINE fromDescListWithKey #-}

-- | /O(n)/. Build a map from a descending list of distinct elements in linear time.
-- /The precondition is not checked./
--
-- > fromDistinctDescList ((5,"a") :| [(3,"b")]) == fromList ((3, "b") :| [(5, "a")])
-- > valid (fromDistinctDescList ((5,"a") :| [(3,"b")]))          == True
-- > valid (fromDistinctDescList ((5,"a") :| [(5,"b"), (3,"b")])) == False
--
-- @since 0.5.8
fromDistinctDescList :: NonEmpty (k, a) -> NEMap k a
fromDistinctDescList ((k, v) :| xs) = insertMapMax k v
                                    . M.fromDistinctDescList
                                    $ xs
{-# INLINE fromDistinctDescList #-}

-- | /O(log n)/. Delete a key and its value from the non-empty map.
-- A potentially empty map ('Map') is returned, since this might delete the
-- last item in the 'NEMap'.  When the key is not a member of the map, is
-- equivalent to 'toMap'.
--
-- > delete 5 (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 3 "b"
-- > delete 7 (fromList ((5,"a") :| [(3,"b")])) == Data.Map.Singleton [(3, "b"), (5, "a")]
delete :: Ord k => k -> NEMap k a -> Map k a
delete k n@(NEMap k0 v m) = case compare k k0 of
    LT -> toMap n
    EQ -> m
    GT -> insertMinMap k0 v . M.delete k $ m
{-# INLINE delete #-}

-- | /O(log n)/. Update a value at a specific key with the result of the
-- provided function. When the key is not a member of the map, the original
-- map is returned.
--
-- > adjust ("new " ++) 5 (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "new a")])
-- > adjust ("new " ++) 7 (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "a")])
adjust
    :: Ord k
    => (a -> a)
    -> k
    -> NEMap k a
    -> NEMap k a
adjust f = adjustWithKey (const f)
{-# INLINE adjust #-}

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "5:new a")])
-- > adjustWithKey f 7 (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "a")])
adjustWithKey
    :: Ord k
    => (k -> a -> a)
    -> k
    -> NEMap k a
    -> NEMap k a
adjustWithKey f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> n
    EQ -> NEMap k0 (f k0 v) m
    GT -> NEMap k0 v . M.adjustWithKey f k $ m
{-# INLINE adjustWithKey #-}

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- Returns a potentially empty map ('Map'), because we can't know ahead of
-- time if the function returns 'Nothing' and deletes the final item in the
-- 'NEMap'.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 5 "a"
update
    :: Ord k
    => (a -> Maybe a)
    -> k
    -> NEMap k a
    -> Map k a
update f = updateWithKey (const f)
{-# INLINE update #-}

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
--
-- Returns a potentially empty map ('Map'), because we can't know ahead of
-- time if the function returns 'Nothing' and deletes the final item in the
-- 'NEMap'.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 5 "a"
updateWithKey
    :: Ord k
    => (k -> a -> Maybe a)
    -> k
    -> NEMap k a
    -> Map k a
updateWithKey f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> toMap n
    EQ -> maybe m (flip (insertMinMap k0) m) . f k0 $ v
    GT -> insertMinMap k0 v . M.updateWithKey f k   $ m
{-# INLINE updateWithKey #-}

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
--
-- Returns a potentially empty map ('Map') in the case that we delete the
-- final key of a singleton map.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList ((5,"a") :| [(3,"b")])) == (Just "5:new a", Data.Map.fromList ((3, "b") :| [(5, "5:new a")]))
-- > updateLookupWithKey f 7 (fromList ((5,"a") :| [(3,"b")])) == (Nothing,  Data.Map.fromList ((3, "b") :| [(5, "a")]))
-- > updateLookupWithKey f 3 (fromList ((5,"a") :| [(3,"b")])) == (Just "b", Data.Map.singleton 5 "a")
updateLookupWithKey
    :: Ord k
    => (k -> a -> Maybe a)
    -> k
    -> NEMap k a
    -> (Maybe a, Map k a)
updateLookupWithKey f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> (Nothing, toMap n)
    EQ -> let u = f k0 v
          in  (u <|> Just v, maybe m (flip (insertMinMap k0) m) u)
    GT -> fmap (insertMinMap k0 v) . M.updateLookupWithKey f k $ m
{-# INLINE updateLookupWithKey #-}

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at
-- @k@, or absence thereof. 'alter' can be used to insert, delete, or
-- update a value in a 'Map'. In short : @Data.Map.lookup k ('alter'
-- f k m) = f ('lookup' k m)@.
--
-- Returns a potentially empty map ('Map'), because we can't know ahead of
-- time if the function returns 'Nothing' and deletes the final item in the
-- 'NEMap'.
--
-- See 'alterF'' for a version that disallows deletion, and so therefore
-- can return 'NEMap'.
--
-- > let f _ = Nothing
-- > alter f 7 (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3, "b"), (5, "a")]
-- > alter f 5 (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 3 "b"
-- >
-- > let f _ = Just "c"
-- > alter f 7 (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3, "b"), (5, "a"), (7, "c")]
-- > alter f 5 (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3, "b"), (5, "c")]
alter
    :: Ord k
    => (Maybe a -> Maybe a)
    -> k
    -> NEMap k a
    -> Map k a
alter f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> ($ toMap n) . maybe id (insertMinMap k ) $ f Nothing
    EQ -> ($ m      ) . maybe id (insertMinMap k0) $ f (Just v)
    GT -> insertMinMap k0 v . M.alter f k $ m
{-# INLINE alter #-}

-- | /O(log n)/. The expression (@'alterF' f k map@) alters the value @x@
-- at @k@, or absence thereof.  'alterF' can be used to inspect, insert,
-- delete, or update a value in a 'Map'.  In short: @Data.Map.lookup
-- k \<$\> 'alterF' f k m = f ('lookup' k m)@.
--
-- Example:
--
-- @
-- interactiveAlter :: Int -> NEMap Int String -> IO (Map Int String)
-- interactiveAlter k m = alterF f k m where
--   f Nothing = do
--      putStrLn $ show k ++
--          " was not found in the map. Would you like to add it?"
--      getUserResponse1 :: IO (Maybe String)
--   f (Just old) = do
--      putStrLn $ "The key is currently bound to " ++ show old ++
--          ". Would you like to change or delete it?"
--      getUserResponse2 :: IO (Maybe String)
-- @
--
-- Like @Data.Map.alterF@ for 'Map', 'alterF' can be considered
-- to be a unifying generalization of 'lookup' and 'delete'; however, as
-- a constrast, it cannot be used to implement 'insert', because it must
-- return a 'Map' instead of an 'NEMap' (because the function might delete
-- the final item in the 'NEMap').  When used with trivial functors like
-- 'Identity' and 'Const', it is often slightly slower than
-- specialized 'lookup' and 'delete'. However, when the functor is
-- non-trivial and key comparison is not particularly cheap, it is the
-- fastest way.
--
-- See 'alterF'' for a version that disallows deletion, and so therefore
-- can return 'NEMap' and be used to implement 'insert'
--
-- Note on rewrite rules:
--
-- This module includes GHC rewrite rules to optimize 'alterF' for
-- the 'Const' and 'Identity' functors. In general, these rules
-- improve performance. The sole exception is that when using
-- 'Identity', deleting a key that is already absent takes longer
-- than it would without the rules. If you expect this to occur
-- a very large fraction of the time, you might consider using a
-- private copy of the 'Identity' type.
--
-- Note: Unlike @Data.Map.alterF@ for 'Map', 'alterF' is /not/ a flipped
-- version of the 'Control.Lens.At.at' combinator from "Control.Lens.At".
-- However, it match the shape expected from most functions expecting
-- lenses, getters, and setters, so can be thought of as a "psuedo-lens",
-- with virtually the same practical applications as a legitimate lens.
alterF
    :: (Ord k, Functor f)
    => (Maybe a -> f (Maybe a))
    -> k
    -> NEMap k a
    -> f (Map k a)
alterF f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> ($ toMap n) . maybe id (insertMinMap k ) <$> f Nothing
    EQ -> ($ m      ) . maybe id (insertMinMap k0) <$> f (Just v)
    GT -> insertMinMap k0 v <$> M.alterF f k m
{-# INLINABLE [2] alterF #-}

-- if f ~ Const b, it's a lookup
{-# RULES
"alterF/Const" forall k (f :: Maybe a -> Const b (Maybe a)) . alterF f k = \m -> Const . getConst . f $ lookup k m
 #-}
-- if f ~ Identity, it's an 'alter'
{-# RULES
"alterF/Identity" forall k (f :: Maybe a -> Identity (Maybe a)) . alterF f k = Identity . alter (runIdentity . f) k
 #-}

-- | /O(log n)/. Variant of 'alter' that disallows deletion.  Allows us to
-- guarantee that the result is also a non-empty Map.
alter'
    :: Ord k
    => (Maybe a -> a)
    -> k
    -> NEMap k a
    -> NEMap k a
alter' f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> NEMap k  (f Nothing) . toMap      $ n
    EQ -> NEMap k0 (f (Just v))             $ m
    GT -> NEMap k0 v . M.alter (Just . f) k $ m
{-# INLINE alter' #-}

-- | /O(log n)/. Variant of 'alterF' that disallows deletion.  Allows us to
-- guarantee that the result is also a non-empty Map.
--
-- Like @Data.Map.alterF@ for 'Map', can be used to generalize and unify
-- 'lookup' and 'insert'.  However, because it disallows deletion, it
-- cannot be used to implement 'delete'.
--
-- See 'alterF' for usage information and caveats.
--
-- Note: Neither 'alterF' nor 'alterF'' can be considered flipped versions
-- of the 'Control.Lens.At.at' combinator from "Control.Lens.At".  However,
-- this can match the shape expected from most functions expecting lenses,
-- getters, and setters, so can be thought of as a "psuedo-lens", with
-- virtually the same practical applications as a legitimate lens.
--
-- __WARNING__: The rewrite rule for 'Identity' exposes an inconsistency in
-- undefined behavior for "Data.Map".  @Data.Map.alterF@ will actually
-- /maintain/ the original key in the map when used with 'Identity';
-- however, @Data.Map.insertWith@ will /replace/ the orginal key in the
-- map.  The rewrite rule for 'alterF'' has chosen to be faithful to
-- @Data.Map.insertWith@, and /not/ @Data.Map.alterF@, for the sake of
-- a cleaner implementation.
alterF'
    :: (Ord k, Functor f)
    => (Maybe a -> f a)
    -> k
    -> NEMap k a
    -> f (NEMap k a)
alterF' f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> flip (NEMap k ) (toMap n) <$> f Nothing
    EQ -> flip (NEMap k0) m         <$> f (Just v)
    GT -> NEMap k0 v <$> M.alterF (fmap Just . f) k m
{-# INLINABLE [2] alterF' #-}

-- if f ~ Const b, it's a lookup
{-# RULES
"alterF'/Const" forall k (f :: Maybe a -> Const b a) . alterF' f k = \m -> Const . getConst . f $ lookup k m
 #-}
-- if f ~ Identity, it's an insertWith
{-# RULES
"alterF'/Identity" forall k (f :: Maybe a -> Identity a) . alterF' f k = Identity . insertWith (\_ -> runIdentity . f . Just) k (runIdentity (f Nothing))
 #-}

-- | /O(n)/. Traverse keys\/values and collect the 'Just' results.
--
-- Returns a potentially empty map ('Map'), our function might return
-- 'Nothing' on every item in the 'NEMap'.
--
-- /Use 'traverseMaybeWithKey1'/ whenever possible (if your 'Applicative'
-- also has 'Apply' instance).  This version is provided only for types
-- that do not have 'Apply' instance, since 'Apply' is not at the moment
-- (and might not ever be) an official superclass of 'Applicative'.
traverseMaybeWithKey
    :: Applicative t
    => (k -> a -> t (Maybe b))
    -> NEMap k a
    -> t (Map k b)
traverseMaybeWithKey f (NEMap k0 v m0) =
    combine <$> f k0 v <*> M.traverseMaybeWithKey f m0
  where
    combine Nothing   = id
    combine (Just v') = insertMinMap k0 v'
{-# INLINE traverseMaybeWithKey #-}

-- | /O(n)/. Traverse keys\/values and collect the 'Just' results.
--
-- Returns a potentially empty map ('Map'), our function might return
-- 'Nothing' on every item in the 'NEMap'.
--
-- Is more general than 'traverseWithKey', since works with all 'Apply',
-- and not just 'Applicative'.

-- TODO: benchmark against M.maxView version
traverseMaybeWithKey1
    :: Apply t
    => (k -> a -> t (Maybe b))
    -> NEMap k a
    -> t (Map k b)
traverseMaybeWithKey1 f (NEMap k0 v m0) = case runMaybeApply m1 of
    Left  m2 -> combine <$> f k0 v <.> m2
    Right m2 -> (`combine` m2) <$> f k0 v
  where
    m1 = M.traverseMaybeWithKey (\k -> MaybeApply . Left . f k) m0
    combine Nothing   = id
    combine (Just v') = insertMinMap k0 v'
{-# INLINE traverseMaybeWithKey1 #-}

-- | /O(n)/. The function 'mapAccum' threads an accumulating argument
-- through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList ((5,"a") :| [(3,"b")])) == ("Everything: ba", fromList ((3, "bX") :| [(5, "aX")]))
mapAccum
    :: (a -> b -> (a, c))
    -> a
    -> NEMap k b
    -> (a, NEMap k c)
mapAccum f = mapAccumWithKey (\x _ -> f x)
{-# INLINE mapAccum #-}

-- | /O(n)/. The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList ((5,"a") :| [(3,"b")])) == ("Everything: 3-b 5-a", fromList ((3, "bX") :| [(5, "aX")]))
mapAccumWithKey
    :: (a -> k -> b -> (a, c))
    -> a
    -> NEMap k b
    -> (a, NEMap k c)
mapAccumWithKey f z0 (NEMap k v m) = (z2, NEMap k v' m')
  where
    ~(z1, v') = f z0 k v
    ~(z2, m') = M.mapAccumWithKey f z1 m
{-# INLINE mapAccumWithKey #-}

-- | /O(n)/. The function 'mapAccumRWithKey' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey
    :: (a -> k -> b -> (a, c))
    -> a
    -> NEMap k b
    -> (a, NEMap k c)
mapAccumRWithKey f z0 (NEMap k v m) = (z2, NEMap k v' m')
  where
    ~(z1, m') = M.mapAccumRWithKey f z0 m
    ~(z2, v') = f z1 k v
{-# INLINE mapAccumRWithKey #-}
-- TODO: what other situations can we take advantage of lazy tuple pattern
-- matching?

-- | /O(n*log n)/.
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the value at the greatest of the
-- original keys is retained.
--
-- While the size of the result map may be smaller than the input map, the
-- output map is still guaranteed to be non-empty if the input map is
-- non-empty.
--
-- > mapKeys (+ 1) (fromList ((5,"a") :| [(3,"b")]))                        == fromList ((4, "b") :| [(6, "a")])
-- > mapKeys (\ _ -> 1) (fromList ((1,"b") :| [(2,"a"), (3,"d"), (4,"c")])) == singleton 1 "c"
-- > mapKeys (\ _ -> 3) (fromList ((1,"b") :| [(2,"a"), (3,"d"), (4,"c")])) == singleton 3 "c"
mapKeys
    :: Ord k2
    => (k1 -> k2)
    -> NEMap k1 a
    -> NEMap k2 a
mapKeys f (NEMap k0 v0 m) = fromListWith const
                          . ((f k0, v0) :|)
                          . M.foldrWithKey (\k v kvs -> (f k, v) : kvs) []
                          $ m
{-# INLINABLE mapKeys #-}

-- | /O(n*log n)/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@. The value at the greater of the two original keys
-- is used as the first argument to @c@.
--
-- While the size of the result map may be smaller than the input map, the
-- output map is still guaranteed to be non-empty if the input map is
-- non-empty.
--
-- > mapKeysWith (++) (\ _ -> 1) (fromList ((1,"b") :| [(2,"a"), (3,"d"), (4,"c")])) == singleton 1 "cdab"
-- > mapKeysWith (++) (\ _ -> 3) (fromList ((1,"b") :| [(2,"a"), (3,"d"), (4,"c")])) == singleton 3 "cdab"
mapKeysWith
    :: Ord k2
    => (a -> a -> a)
    -> (k1 -> k2)
    -> NEMap k1 a
    -> NEMap k2 a
mapKeysWith c f (NEMap k0 v0 m) = fromListWith c
                                . ((f k0, v0) :|)
                                . M.foldrWithKey (\k v kvs -> (f k, v) : kvs) []
                                $ m
{-# INLINABLE mapKeysWith #-}

-- | /O(n)/.
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has better performance than 'mapKeys'.
--
-- While the size of the result map may be smaller than the input map, the
-- output map is still guaranteed to be non-empty if the input map is
-- non-empty.
--
-- > mapKeysMonotonic (\ k -> k * 2) (fromList ((5,"a") :| [(3,"b")])) == fromList ((6, "b") :| [(10, "a")])
-- > valid (mapKeysMonotonic (\ k -> k * 2) (fromList ((5,"a") :| [(3,"b")]))) == True
-- > valid (mapKeysMonotonic (\ _ -> 1)     (fromList ((5,"a") :| [(3,"b")]))) == False
mapKeysMonotonic
    :: (k1 -> k2)
    -> NEMap k1 a
    -> NEMap k2 a
mapKeysMonotonic f (NEMap k v m) = NEMap (f k) v
                                 . M.mapKeysMonotonic f
                                 $ m
{-# INLINE mapKeysMonotonic #-}

-- | /O(n)/. Filter all values that satisfy the predicate.
--
-- Returns a potentially empty map ('Map'), because we could
-- potentailly filter out all items in the original 'NEMap'.
--
-- > filter (> "a") (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 3 "b"
-- > filter (> "x") (fromList ((5,"a") :| [(3,"b")])) == Data.Map.empty
-- > filter (< "a") (fromList ((5,"a") :| [(3,"b")])) == Data.Map.empty
filter
    :: (a -> Bool)
    -> NEMap k a
    -> Map k a
filter f (NEMap k v m)
    | f v       = insertMinMap k v . M.filter f $ m
    | otherwise = M.filter f m
{-# INLINE filter #-}

-- | /O(n)/. Filter all keys\/values that satisfy the predicate.
--
-- Returns a potentially empty map ('Map'), because we could
-- potentailly filter out all items in the original 'NEMap'.
--
-- > filterWithKey (\k _ -> k > 4) (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 5 "a"
filterWithKey
    :: (k -> a -> Bool)
    -> NEMap k a
    -> Map k a
filterWithKey f (NEMap k v m)
    | f k v     = insertMinMap k v . M.filterWithKey f $ m
    | otherwise = M.filterWithKey f m
{-# INLINE filterWithKey #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Restrict an 'NEMap' to only those keys
-- found in a 'Data.Set.Set'.
--
-- @
-- m \`restrictKeys\` s = 'filterWithKey' (\k _ -> k ``Set.member`` s) m
-- m \`restrictKeys\` s = m ``intersection`` 'fromSet' (const ()) s
-- @
restrictKeys
    :: Ord k
    => NEMap k a
    -> Set k
    -> Map k a
restrictKeys n@(NEMap k v m) xs = case S.minView xs of
    Nothing      -> M.empty
    Just (y, ys) -> case compare k y of
      -- k is not in xs
      LT -> m `M.restrictKeys` xs
      -- k and y are a part of the result
      EQ -> insertMinMap k v $ m `M.restrictKeys` ys
      -- y is not in m
      GT -> toMap n `M.restrictKeys` ys
{-# INLINE restrictKeys #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Remove all keys in a 'Data.Set.Set' from
-- an 'NEMap'.
--
-- @
-- m \`withoutKeys\` s = 'filterWithKey' (\k _ -> k ``Set.notMember`` s) m
-- m \`withoutKeys\` s = m ``difference`` 'fromSet' (const ()) s
-- @
withoutKeys
    :: Ord k
    => NEMap k a
    -> Set k
    -> Map k a
withoutKeys n@(NEMap k v m) xs = case S.minView xs of
    Nothing      -> toMap n
    Just (y, ys) -> case compare k y of
      -- k is not in xs, so cannot be deleted
      LT -> insertMinMap k v $ m `M.withoutKeys` xs
      -- y deletes k, and only k
      EQ -> m `M.withoutKeys` ys
      -- y is not in n, so cannot delete anything, so we can just difference n and ys
      GT -> toMap n `M.withoutKeys` ys
{-# INLINE withoutKeys #-}

-- | /O(n)/. Partition the map according to a predicate.
--
-- Returns a 'These' with potentially two non-empty maps:
--
-- *   @'This' n1@ means that the predicate was true for all items.
-- *   @'That' n2@ means that the predicate was false for all items.
-- *   @'These' n1 n2@ gives @n1@ (all of the items that were true for the
--     predicate) and @n2@ (all of the items that were false for the
--     predicate).
--
-- See also 'split'.
--
-- > partition (> "a") (fromList ((5,"a") :| [(3,"b")])) == These (singleton 3 "b") (singleton 5 "a")
-- > partition (< "x") (fromList ((5,"a") :| [(3,"b")])) == This  (fromList ((3, "b") :| [(5, "a")]))
-- > partition (> "x") (fromList ((5,"a") :| [(3,"b")])) == That  (fromList ((3, "b") :| [(5, "a")]))
partition
    :: (a -> Bool)
    -> NEMap k a
    -> These (NEMap k a) (NEMap k a)
partition f = partitionWithKey (const f)
{-# INLINE partition #-}

-- | /O(n)/. Partition the map according to a predicate.
--
-- Returns a 'These' with potentially two non-empty maps:
--
-- *   @'This' n1@ means that the predicate was true for all items,
--     returning the original map.
-- *   @'That' n2@ means that the predicate was false for all items,
--     returning the original map.
-- *   @'These' n1 n2@ gives @n1@ (all of the items that were true for the
--     predicate) and @n2@ (all of the items that were false for the
--     predicate).
--
-- See also 'split'.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList ((5,"a") :| [(3,"b")])) == These (singleton 5 "a") (singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList ((5,"a") :| [(3,"b")])) == This  (fromList ((3, "b") :| [(5, "a")]))
-- > partitionWithKey (\ k _ -> k > 7) (fromList ((5,"a") :| [(3,"b")])) == That  (fromList ((3, "b") :| [(5, "a")]))
partitionWithKey
    :: (k -> a -> Bool)
    -> NEMap k a
    -> These (NEMap k a) (NEMap k a)
partitionWithKey f n@(NEMap k v m0) = case (nonEmptyMap m1, nonEmptyMap m2) of
    (Nothing, Nothing)
      | f k v     -> This  n
      | otherwise -> That                        n
    (Just n1, Nothing)
      | f k v     -> This  n
      | otherwise -> These n1                    (singleton k v)
    (Nothing, Just n2)
      | f k v     -> These (singleton k v)       n2
      | otherwise -> That                        n
    (Just n1, Just n2)
      | f k v     -> These (insertMapMin k v m1) n2
      | otherwise -> These n1                    (insertMapMin k v m2)
  where
    (m1, m2) = M.partitionWithKey f m0
{-# INLINABLE partitionWithKey #-}

-- | /O(log n)/. Take while a predicate on the keys holds.
-- The user is responsible for ensuring that for all keys @j@ and @k@ in the map,
-- @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.
--
-- Returns a potentially empty map ('Map'), because the predicate might
-- fail on the first input.
--
-- @
-- takeWhileAntitone p = Data.Map.fromDistinctAscList . Data.List.takeWhile (p . fst) . Data.Foldable.toList
-- takeWhileAntitone p = 'filterWithKey' (\k _ -> p k)
-- @
takeWhileAntitone
    :: (k -> Bool)
    -> NEMap k a
    -> Map k a
takeWhileAntitone f (NEMap k v m)
    | f k       = insertMinMap k v . M.takeWhileAntitone f $ m
    | otherwise = M.empty
{-# INLINE takeWhileAntitone #-}

-- | /O(log n)/. Drop while a predicate on the keys holds.
-- The user is responsible for ensuring that for all keys @j@ and @k@ in the map,
-- @j \< k ==\> p j \>= p k@. See note at 'spanAntitone'.
--
-- @
-- dropWhileAntitone p = Data.Map.fromDistinctAscList . Data.List.dropWhile (p . fst) . Data.Foldable.toList
-- dropWhileAntitone p = 'filterWithKey' (\k -> not (p k))
-- @
dropWhileAntitone
    :: (k -> Bool)
    -> NEMap k a
    -> Map k a
dropWhileAntitone f n@(NEMap k _ m)
    | f k       = M.dropWhileAntitone f m
    | otherwise = toMap n
{-# INLINE dropWhileAntitone #-}

-- | /O(log n)/. Divide a map at the point where a predicate on the keys stops holding.
-- The user is responsible for ensuring that for all keys @j@ and @k@ in the map,
-- @j \< k ==\> p j \>= p k@.
--
-- Returns a 'These' with potentially two non-empty maps:
--
-- *   @'This' n1@ means that the predicate never failed for any item,
--     returning the original map.
-- *   @'That' n2@ means that the predicate failed for the first item,
--     returning the original map.
-- *   @'These' n1 n2@ gives @n1@ (the map up to the point where the
--     predicate on the keys stops holding) and @n2@ (the map starting from
--     the point where the predicate stops holding)
--
-- @
-- spanAntitone p xs = partitionWithKey (\k _ -> p k) xs
-- @
--
-- Note: if @p@ is not actually antitone, then @spanAntitone@ will split the map
-- at some /unspecified/ point where the predicate switches from holding to not
-- holding (where the predicate is seen to hold before the first key and to fail
-- after the last key).
spanAntitone
    :: (k -> Bool)
    -> NEMap k a
    -> These (NEMap k a) (NEMap k a)
spanAntitone f n@(NEMap k v m0)
    | f k       = case (nonEmptyMap m1, nonEmptyMap m2) of
        (Nothing, Nothing) -> This  n
        (Just _ , Nothing) -> This  n
        (Nothing, Just n2) -> These (singleton k v)       n2
        (Just _ , Just n2) -> These (insertMapMin k v m1) n2
    | otherwise = That n
  where
    (m1, m2) = M.spanAntitone f m0
{-# INLINABLE spanAntitone #-}

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- Returns a potentially empty map ('Map'), because the function could
-- potentially return 'Nothing' on all items in the 'NEMap'.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 5 "new a"
mapMaybe
    :: (a -> Maybe b)
    -> NEMap k a
    -> Map k b
mapMaybe f = mapMaybeWithKey (const f)
{-# INLINE mapMaybe #-}

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- Returns a potentially empty map ('Map'), because the function could
-- potentially return 'Nothing' on all items in the 'NEMap'.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 3 "key : 3"
mapMaybeWithKey
    :: (k -> a -> Maybe b)
    -> NEMap k a
    -> Map k b
mapMaybeWithKey f (NEMap k v m) = ($ M.mapMaybeWithKey f m)
                                . maybe id (insertMinMap k)
                                $ f k v
{-# INLINE mapMaybeWithKey #-}

-- | /O(n)/. Map values and separate the 'Left' and 'Right' results.
--
-- Returns a 'These' with potentially two non-empty maps:
--
-- *   @'This' n1@ means that the results were all 'Left'.
-- *   @'That' n2@ means that the results were all 'Right'.
-- *   @'These' n1 n2@ gives @n1@ (the map where the results were 'Left')
--     and @n2@ (the map where the results were 'Right')
--
-- > let f a = if a < "c" then Left a else Right a
-- > mapEither f (fromList ((5,"a") :| [(3,"b"), (1,"x"), (7,"z")]))
-- >     == These (fromList ((3,"b") :| [(5,"a")])) (fromList ((1,"x") :| [(7,"z")]))
-- >
-- > mapEither (\ a -> Right a) (fromList ((5,"a") :| [(3,"b"), (1,"x"), (7,"z")]))
-- >     == That (fromList ((5,"a") :| [(3,"b"), (1,"x"), (7,"z")]))
mapEither
    :: (a -> Either b c)
    -> NEMap k a
    -> These (NEMap k b) (NEMap k c)
mapEither f = mapEitherWithKey (const f)
{-# INLINE mapEither #-}

-- | /O(n)/. Map keys\/values and separate the 'Left' and 'Right' results.
--
-- Returns a 'These' with potentially two non-empty maps:
--
-- *   @'This' n1@ means that the results were all 'Left'.
-- *   @'That' n2@ means that the results were all 'Right'.
-- *   @'These' n1 n2@ gives @n1@ (the map where the results were 'Left')
--     and @n2@ (the map where the results were 'Right')
--
-- > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
-- > mapEitherWithKey f (fromList ((5,"a") :| [(3,"b"), (1,"x"), (7,"z")]))
-- >     == These (fromList ((1,2) :| [(3,6)])) (fromList ((5,"aa") :| [(7,"zz")]))
-- >
-- > mapEitherWithKey (\_ a -> Right a) (fromList ((5,"a") :| [(3,"b"), (1,"x"), (7,"z")]))
-- >     == That (fromList ((1,"x") :| [(3,"b"), (5,"a"), (7,"z")]))
mapEitherWithKey
    :: (k -> a -> Either b c)
    -> NEMap k a
    -> These (NEMap k b) (NEMap k c)
mapEitherWithKey f (NEMap k v m0) = case (nonEmptyMap m1, nonEmptyMap m2) of
    (Nothing, Nothing) -> case f k v of
      Left  v' -> This  (singleton k v')
      Right v' -> That                         (singleton k v')
    (Just n1, Nothing) -> case f k v of
      Left  v' -> This  (insertMapMin k v' m1)
      Right v' -> These n1                     (singleton k v')
    (Nothing, Just n2) -> case f k v of
      Left  v' -> These (singleton k v')       n2
      Right v' -> That                         (insertMapMin k v' m2)
    (Just n1, Just n2) -> case f k v of
      Left  v' -> These (insertMapMin k v' m1) n2
      Right v' -> These n1                     (insertMapMin k v' m2)
  where
    (m1, m2) = M.mapEitherWithKey f m0
{-# INLINABLE mapEitherWithKey #-}

-- | /O(log n)/. The expression (@'split' k map@) is potentially a 'These'
-- containing up to two 'NEMap's based on splitting the map into maps
-- containing items before and after the given key @k@.  It will never
-- return a map that contains @k@ itself.
--
-- *   'Nothing' means that @k@ was the only key in the the original map,
--     and so there are no items before or after it.
-- *   @'Just' ('This' n1)@ means @k@ was larger than or equal to all items
--     in the map, and @n1@ is the entire original map (minus @k@, if it was
--     present)
-- *   @'Just' ('That' n2)@ means @k@ was smaller than or equal to all
--     items in the map, and @n2@ is the entire original map (minus @k@, if
--     it was present)
-- *   @'Just' ('These' n1 n2)@ gives @n1@ (the map of all keys from the
--     original map less than @k@) and @n2@ (the map of all keys from the
--     original map greater than @k@)
--
-- > split 2 (fromList ((5,"a") :| [(3,"b")])) == Just (That  (fromList ((3,"b") :| [(5,"a")]))  )
-- > split 3 (fromList ((5,"a") :| [(3,"b")])) == Just (That  (singleton 5 "a")                  )
-- > split 4 (fromList ((5,"a") :| [(3,"b")])) == Just (These (singleton 3 "b") (singleton 5 "a"))
-- > split 5 (fromList ((5,"a") :| [(3,"b")])) == Just (This  (singleton 3 "b")                  )
-- > split 6 (fromList ((5,"a") :| [(3,"b")])) == Just (This  (fromList ((3,"b") :| [(5,"a")]))  )
-- > split 5 (singleton 5 "a")                 == Nothing
split
    :: Ord k
    => k
    -> NEMap k a
    -> Maybe (These (NEMap k a) (NEMap k a))
split k n@(NEMap k0 v m0) = case compare k k0 of
    LT -> Just $ That n
    EQ -> That <$> nonEmptyMap m0
    GT -> Just $ case (nonEmptyMap m1, nonEmptyMap m2) of
      (Nothing, Nothing) -> This  (singleton k0 v)
      (Just _ , Nothing) -> This  (insertMapMin k0 v m1)
      (Nothing, Just n2) -> These (singleton k0 v)       n2
      (Just _ , Just n2) -> These (insertMapMin k0 v m1) n2
  where
    (m1, m2) = M.split k m0
{-# INLINABLE split #-}

-- | /O(log n)/. The expression (@'splitLookup' k map@) splits a map just
-- like 'split' but also returns @'lookup' k map@, as the first field in
-- the 'These':
--
-- > splitLookup 2 (fromList ((5,"a") :| [(3,"b")])) == That      (That  (fromList ((3,"b") :| [(5,"a")])))
-- > splitLookup 3 (fromList ((5,"a") :| [(3,"b")])) == These "b" (That  (singleton 5 "a"))
-- > splitLookup 4 (fromList ((5,"a") :| [(3,"b")])) == That      (These (singleton 3 "b") (singleton 5 "a"))
-- > splitLookup 5 (fromList ((5,"a") :| [(3,"b")])) == These "a" (This  (singleton 3 "b"))
-- > splitLookup 6 (fromList ((5,"a") :| [(3,"b")])) == That      (This  (fromList ((3,"b") :| [(5,"a")])))
-- > splitLookup 5 (singleton 5 "a")                 == This  "a"
splitLookup
    :: Ord k
    => k
    -> NEMap k a
    -> These a (These (NEMap k a) (NEMap k a))
splitLookup k n@(NEMap k0 v0 m0) = case compare k k0 of
    LT -> That . That $ n
    EQ -> maybe (This v0) (These v0 . That) . nonEmptyMap $ m0
    GT -> maybe That These v $ case (nonEmptyMap m1, nonEmptyMap m2) of
      (Nothing, Nothing) -> This  (singleton k0 v0)
      (Just _ , Nothing) -> This  (insertMapMin k0 v0 m1)
      (Nothing, Just n2) -> These (singleton k0 v0)       n2
      (Just _ , Just n2) -> These (insertMapMin k0 v0 m1) n2
  where
    (m1, v, m2) = M.splitLookup k m0
{-# INLINABLE splitLookup #-}

-- | /O(1)/.  Decompose a map into pieces based on the structure of the
-- underlying tree.  This function is useful for consuming a map in
-- parallel.
--
-- No guarantee is made as to the sizes of the pieces; an internal, but
-- deterministic process determines this.  However, it is guaranteed that
-- the pieces returned will be in ascending order (all elements in the
-- first submap less than all elements in the second, and so on).
--
-- Note that the current implementation does not return more than four
-- submaps, but you should not depend on this behaviour because it can
-- change in the future without notice.
splitRoot
    :: NEMap k a
    -> NonEmpty (NEMap k a)
splitRoot (NEMap k v m) = singleton k v
                       :| Maybe.mapMaybe nonEmptyMap (M.splitRoot m)
{-# INLINE splitRoot #-}

-- | /O(m*log(n\/m + 1)), m <= n/.
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: (Ord k, Eq a) => NEMap k a -> NEMap k a -> Bool
isSubmapOf = isSubmapOfBy (==)
{-# INLINE isSubmapOf #-}

-- | /O(m*log(n\/m + 1)), m <= n/.
-- The expression (@'isSubmapOfBy' f t1 t2@) returns 'True' if
-- all keys in @t1@ are in tree @t2@, and when @f@ returns 'True' when
-- applied to their respective values. For example, the following
-- expressions are all 'True':
--
-- > isSubmapOfBy (==) (singleton 'a' 1) (fromList (('a',1) :| [('b',2)]))
-- > isSubmapOfBy (<=) (singleton 'a' 1) (fromList (('a',1) :| [('b',2)]))
-- > isSubmapOfBy (==) (fromList (('a',1) :| [('b',2)])) (fromList (('a',1) :| [('b',2)]))
--
-- But the following are all 'False':
--
-- > isSubmapOfBy (==) (singleton 'a' 2) (fromList (('a',1) :| [('b',2)]))
-- > isSubmapOfBy (<)  (singleton 'a' 1) (fromList (('a',1) :| [('b',2)]))
-- > isSubmapOfBy (==) (fromList (('a',1) :| [('b',2)])) (singleton 'a' 1)
isSubmapOfBy
    :: Ord k
    => (a -> b -> Bool)
    -> NEMap k a
    -> NEMap k b
    -> Bool
isSubmapOfBy f (NEMap k v m0) (toMap->m1) = kvSub
                                         && M.isSubmapOfBy f m0 m1
  where
    kvSub = case M.lookup k m1 of
      Just v0 -> f v v0
      Nothing -> False
{-# INLINE isSubmapOfBy #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Is this a proper submap? (ie. a submap
-- but not equal). Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy'
-- (==)@).
isProperSubmapOf :: (Ord k, Eq a) => NEMap k a -> NEMap k a -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)
{-# INLINE isProperSubmapOf #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Is this a proper submap? (ie. a submap
-- but not equal). The expression (@'isProperSubmapOfBy' f m1 m2@) returns
-- 'True' when @m1@ and @m2@ are not equal, all keys in @m1@ are in @m2@,
-- and when @f@ returns 'True' when applied to their respective values. For
-- example, the following expressions are all 'True':
--
--  > isProperSubmapOfBy (==) (singleton 1 1) (fromList ((1,1) :| [(2,2)]))
--  > isProperSubmapOfBy (<=) (singleton 1 1) (fromList ((1,1) :| [(2,2)]))
--
-- But the following are all 'False':
--
--  > isProperSubmapOfBy (==) (fromList ((1,1) :| [(2,2)])) (fromList ((1,1) :| [(2,2)]))
--  > isProperSubmapOfBy (==) (fromList ((1,1) :| [(2,2)])) (singleton 1 1))
--  > isProperSubmapOfBy (<)  (singleton 1 1)               (fromList ((1,1) :| [(2,2)]))
isProperSubmapOfBy
    :: Ord k
    => (a -> b -> Bool)
    -> NEMap k a
    -> NEMap k b
    -> Bool
isProperSubmapOfBy f m1 m2 = M.size (nemMap m1) < M.size (nemMap m2)
                          && isSubmapOfBy f m1 m2
{-# INLINE isProperSubmapOfBy #-}

-- | /O(log n)/. Lookup the /index/ of a key, which is its zero-based index
-- in the sequence sorted by keys. The index is a number from /0/ up to,
-- but not including, the 'size' of the map.
--
-- > isJust (lookupIndex 2 (fromList ((5,"a") :| [(3,"b")])))   == False
-- > fromJust (lookupIndex 3 (fromList ((5,"a") :| [(3,"b")]))) == 0
-- > fromJust (lookupIndex 5 (fromList ((5,"a") :| [(3,"b")]))) == 1
-- > isJust (lookupIndex 6 (fromList ((5,"a") :| [(3,"b")])))   == False
lookupIndex
    :: Ord k
    => k
    -> NEMap k a
    -> Maybe Int
lookupIndex k (NEMap k0 _ m) = case compare k k0 of
    LT -> Nothing
    EQ -> Just 0
    GT -> (+ 1) <$> M.lookupIndex k m
{-# INLINE lookupIndex #-}

-- | /O(log n)/. Return the /index/ of a key, which is its zero-based index
-- in the sequence sorted by keys. The index is a number from /0/ up to,
-- but not including, the 'size' of the map. Calls 'error' when the key is
-- not a 'member' of the map.
--
-- > findIndex 2 (fromList ((5,"a") :| [(3,"b")]))    Error: element is not in the map
-- > findIndex 3 (fromList ((5,"a") :| [(3,"b")])) == 0
-- > findIndex 5 (fromList ((5,"a") :| [(3,"b")])) == 1
-- > findIndex 6 (fromList ((5,"a") :| [(3,"b")]))    Error: element is not in the map
findIndex
    :: Ord k
    => k
    -> NEMap k a
    -> Int
findIndex k = fromMaybe e . lookupIndex k
  where
    e = error "NEMap.findIndex: element is not in the map"
{-# INLINE findIndex #-}

-- | /O(log n)/. Retrieve an element by its /index/, i.e. by its zero-based
-- index in the sequence sorted by keys. If the /index/ is out of range
-- (less than zero, greater or equal to 'size' of the map), 'error' is
-- called.
--
-- > elemAt 0 (fromList ((5,"a") :| [(3,"b")])) == (3,"b")
-- > elemAt 1 (fromList ((5,"a") :| [(3,"b")])) == (5, "a")
-- > elemAt 2 (fromList ((5,"a") :| [(3,"b")]))    Error: index out of range
elemAt
    :: Int
    -> NEMap k a
    -> (k, a)
elemAt 0 (NEMap k v _) = (k, v)
elemAt i (NEMap _ _ m) = M.elemAt (i - 1) m
{-# INLINABLE elemAt #-}

-- | /O(log n)/. Update the element at /index/, i.e. by its zero-based index in
-- the sequence sorted by keys. If the /index/ is out of range (less than zero,
-- greater or equal to 'size' of the map), 'error' is called.
--
-- Returns a possibly empty map ('Map'), because the function might end up
-- deleting the last key in the map.  See 'adjustAt' for a version that
-- disallows deletion, guaranteeing that the result is also a non-empty
-- Map.
--
-- > updateAt (\ _ _ -> Just "x") 0    (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3, "x"), (5, "a")]
-- > updateAt (\ _ _ -> Just "x") 1    (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3, "b"), (5, "x")]
-- > updateAt (\ _ _ -> Just "x") 2    (fromList ((5,"a") :| [(3,"b")]))    Error: index out of range
-- > updateAt (\ _ _ -> Just "x") (-1) (fromList ((5,"a") :| [(3,"b")]))    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  0    (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 5 "a"
-- > updateAt (\_ _  -> Nothing)  1    (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 3 "b"
-- > updateAt (\_ _  -> Nothing)  2    (fromList ((5,"a") :| [(3,"b")]))    Error: index out of range
-- > updateAt (\_ _  -> Nothing)  (-1) (fromList ((5,"a") :| [(3,"b")]))    Error: index out of range
updateAt
    :: (k -> a -> Maybe a)
    -> Int
    -> NEMap k a
    -> Map k a
updateAt f 0 (NEMap k v m) = maybe m (flip (insertMinMap k) m) $ f k v
updateAt f i (NEMap k v m) = insertMinMap k v . M.updateAt f (i - 1) $ m
{-# INLINABLE updateAt #-}

-- | /O(log n)/. Variant of 'updateAt' that disallows deletion.  Allows us
-- to guarantee that the result is also a non-empty Map.
adjustAt
    :: (k -> a -> a)
    -> Int
    -> NEMap k a
    -> NEMap k a
adjustAt f 0 (NEMap k0 v m) = NEMap k0 (f k0 v) m
adjustAt f i (NEMap k0 v m) = NEMap k0 v
                            . M.updateAt (\k -> Just . f k) (i - 1)
                            $ m
{-# INLINABLE adjustAt #-}

-- | /O(log n)/. Delete the element at /index/, i.e. by its zero-based
-- index in the sequence sorted by keys. If the /index/ is out of range
-- (less than zero, greater or equal to 'size' of the map), 'error' is
-- called.
--
-- Returns a potentially empty map ('Map') because of the possibility of
-- deleting the last item in a map.
--
-- > deleteAt 0  (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 5 "a"
-- > deleteAt 1  (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 3 "b"
-- > deleteAt 2 (fromList ((5,"a") :| [(3,"b")]))     Error: index out of range
-- > deleteAt (-1) (fromList ((5,"a") :| [(3,"b")]))  Error: index out of range
deleteAt
    :: Int
    -> NEMap k a
    -> Map k a
deleteAt 0 (NEMap _ _ m) = m
deleteAt i (NEMap k v m) = insertMinMap k v . M.deleteAt (i - 1) $ m
{-# INLINABLE deleteAt #-}

-- | Take a given number of entries in key order, beginning with the
-- smallest keys.
--
-- Returns a possibly empty map ('Map'), which can only happen if we call
-- @take 0@.
--
-- @
-- take n = Data.Map.fromDistinctAscList . Data.List.NonEmpty.take n . 'toList'
-- @
take
    :: Int
    -> NEMap k a
    -> Map k a
take 0 NEMap{}       = M.empty
take i (NEMap k v m) = insertMinMap k v . M.take (i - 1) $ m
{-# INLINABLE take #-}

-- | Drop a given number of entries in key order, beginning
-- with the smallest keys.
--
-- Returns a possibly empty map ('Map'), in case we drop all of the
-- elements (which can happen if we drop a number greater than or equal to
-- the number of items in the map)
--
-- @
-- drop n = Data.Map.fromDistinctAscList . Data.List.NonEmpty.drop' n . 'toList'
-- @
drop
    :: Int
    -> NEMap k a
    -> Map k a
drop 0 n             = toMap n
drop i (NEMap _ _ m) = M.drop (i - 1) m
{-# INLINABLE drop #-}

-- | /O(log n)/. Split a map at a particular index @i@.
--
-- *   @'This' n1@ means that there are less than @i@ items in the map, and
--     @n1@ is the original map.
-- *   @'That' n2@ means @i@ was 0; we dropped 0 items, so @n2@ is the
--     original map.
-- *   @'These' n1 n2@ gives @n1@ (taking @i@ items from the original map)
--     and @n2@ (dropping @i@ items from the original map))
splitAt
    :: Int
    -> NEMap k a
    -> These (NEMap k a) (NEMap k a)
splitAt 0 n                = That n
splitAt i n@(NEMap k v m0) = case (nonEmptyMap m1, nonEmptyMap m2) of
    (Nothing, Nothing) -> This  (singleton k v)
    (Just _ , Nothing) -> This  n
    (Nothing, Just n2) -> These (singleton k v)       n2
    (Just _ , Just n2) -> These (insertMapMin k v m1) n2
  where
    (m1, m2) = M.splitAt (i - 1) m0
{-# INLINABLE splitAt #-}

-- | /O(1)/. The minimal key of the map.  Note that this is total, making
-- 'Data.Map.lookupMin' obsolete.  It is constant-time, so has better
-- asymptotics than @Data.Map.lookupMin@ and @Data.Map.findMin@, as well.
--
-- > findMin (fromList ((5,"a") :| [(3,"b")])) == (3,"b")
findMin :: NEMap k a -> (k, a)
findMin (NEMap k v _) = (k, v)
{-# INLINE findMin #-}

-- | /O(log n)/. The maximal key of the map.  Note that this is total, making
-- 'Data.Map.lookupMin' obsolete.
--
-- > findMax (fromList ((5,"a") :| [(3,"b")])) == (5,"a")
findMax :: NEMap k a -> (k, a)
findMax (NEMap k v m) = fromMaybe (k, v) . M.lookupMax $ m
{-# INLINE findMax #-}

-- | /O(1)/. Delete the minimal key. Returns a potentially empty map
-- ('Map'), because we might end up deleting the final key in a singleton
-- map.  It is constant-time, so has better asymptotics than
-- 'Data.Map.deleteMin'.
--
-- > deleteMin (fromList ((5,"a") :| [(3,"b"), (7,"c")])) == Data.Map.fromList [(5,"a"), (7,"c")]
-- > deleteMin (singleton 5 "a") == Data.Map.empty
deleteMin :: NEMap k a -> Map k a
deleteMin (NEMap _ _ m) = m
{-# INLINE deleteMin #-}

-- | /O(log n)/. Delete the maximal key. Returns a potentially empty map
-- ('Map'), because we might end up deleting the final key in a singleton
-- map.
--
-- > deleteMax (fromList ((5,"a") :| [(3,"b"), (7,"c")])) == Data.Map.fromList [(3,"b"), (5,"a")]
-- > deleteMax (singleton 5 "a") == Data.Map.empty
deleteMax :: NEMap k a -> Map k a
deleteMax (NEMap k v m) = case M.maxView m of
    Nothing      -> M.empty
    Just (_, m') -> insertMinMap k v m'
{-# INLINE deleteMax #-}

-- | /O(1)/ if delete, /O(log n)/ otherwise. Update the value at the
-- minimal key.  Returns a potentially empty map ('Map'), because we might
-- end up deleting the final key in the map if the function returns
-- 'Nothing'.  See 'adjustMin' for a version that can guaruntee that we
-- return a non-empty map.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 5 "a"
updateMin :: (a -> Maybe a) -> NEMap k a -> Map k a
updateMin f = updateMinWithKey (const f)
{-# INLINE updateMin #-}

-- | /O(1)/. A version of 'updateMin' that disallows deletion, allowing us
-- to guarantee that the result is also non-empty.
adjustMin :: (a -> a) -> NEMap k a -> NEMap k a
adjustMin f = adjustMinWithKey (const f)
{-# INLINE adjustMin #-}

-- | /O(1)/ if delete, /O(log n)/ otherwise. Update the value at the
-- minimal key.  Returns a potentially empty map ('Map'), because we might
-- end up deleting the final key in the map if the function returns
-- 'Nothing'.  See 'adjustMinWithKey' for a version that guaruntees
-- a non-empty map.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 5 "a"
updateMinWithKey :: (k -> a -> Maybe a) -> NEMap k a -> Map k a
updateMinWithKey f (NEMap k v m) = ($ m) . maybe id (insertMinMap k) $ f k v
{-# INLINE updateMinWithKey #-}

-- | /O(1)/. A version of 'adjustMaxWithKey' that disallows deletion,
-- allowing us to guarantee that the result is also non-empty.  Note that
-- it also is able to have better asymptotics than 'updateMinWithKey' in
-- general.
adjustMinWithKey :: (k -> a -> a) -> NEMap k a -> NEMap k a
adjustMinWithKey f (NEMap k v m) = NEMap k (f k v) m
{-# INLINE adjustMinWithKey #-}

-- | /O(log n)/. Update the value at the maximal key.  Returns
-- a potentially empty map ('Map'), because we might end up deleting the
-- final key in the map if the function returns 'Nothing'.  See 'adjustMax'
-- for a version that can guarantee that we return a non-empty map.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 3 "b"
updateMax :: (a -> Maybe a) -> NEMap k a -> Map k a
updateMax f = updateMaxWithKey (const f)
{-# INLINE updateMax #-}

-- | /O(log n)/. A version of 'updateMax' that disallows deletion, allowing
-- us to guarantee that the result is also non-empty.
adjustMax :: (a -> a) -> NEMap k a -> NEMap k a
adjustMax f = adjustMaxWithKey (const f)
{-# INLINE adjustMax #-}

-- | /O(log n)/. Update the value at the maximal key.  Returns
-- a potentially empty map ('Map'), because we might end up deleting the
-- final key in the map if the function returns 'Nothing'. See
-- 'adjustMaxWithKey' for a version that guaruntees a non-empty map.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList ((5,"a") :| [(3,"b")])) == Data.Map.fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList ((5,"a") :| [(3,"b")])) == Data.Map.singleton 5 "a"
updateMaxWithKey :: (k -> a -> Maybe a) -> NEMap k a -> Map k a
updateMaxWithKey f (NEMap k v m)
    | M.null m  = maybe m (M.singleton k) $ f k v
    | otherwise = insertMinMap k v
                . M.updateMaxWithKey f
                $ m
{-# INLINE updateMaxWithKey #-}

-- | /O(log n)/. A version of 'updateMaxWithKey' that disallows deletion,
-- allowing us to guarantee that the result is also non-empty.
adjustMaxWithKey :: (k -> a -> a) -> NEMap k a -> NEMap k a
adjustMaxWithKey f (NEMap k0 v m)
    | M.null m  = NEMap k0 (f k0 v) m
    | otherwise = insertMapMin k0 v
                . M.updateMaxWithKey (\k -> Just . f k)
                $ m
{-# INLINE adjustMaxWithKey #-}

-- | /O(1)/. Retrieves the value associated with minimal key of the
-- map, and the map stripped of that element.  It is constant-time, so has
-- better asymptotics than @Data.Map.minView@ for 'Map'.
--
-- Note that unlike @Data.Map.minView@ for 'Map', this cannot ever fail,
-- so doesn't need to return in a 'Maybe'.  However, the result 'Map' is
-- potentially empty, since the original map might have contained just
-- a single item.
--
-- > minView (fromList ((5,"a") :| [(3,"b")])) == ("b", Data.Map.singleton 5 "a")
minView :: NEMap k a -> (a, Map k a)
minView = first snd . deleteFindMin
{-# INLINE minView #-}

-- | /O(1)/. Delete and find the minimal key-value pair.  It is
-- constant-time, so has better asymptotics that @Data.Map.minView@ for
-- 'Map'.
--
-- Note that unlike @Data.Map.deleteFindMin@ for 'Map', this cannot ever
-- fail, and so is a total function. However, the result 'Map' is
-- potentially empty, since the original map might have contained just
-- a single item.
--
-- > deleteFindMin (fromList ((5,"a") :| [(3,"b"), (10,"c")])) == ((3,"b"), Data.Map.fromList [(5,"a"), (10,"c")])
deleteFindMin :: NEMap k a -> ((k, a), Map k a)
deleteFindMin (NEMap k v m) = ((k, v), m)
{-# INLINE deleteFindMin #-}

-- | /O(log n)/. Retrieves the value associated with maximal key of the
-- map, and the map stripped of that element.
--
-- Note that unlike @Data.Map.maxView@ from 'Map', this cannot ever fail,
-- so doesn't need to return in a 'Maybe'.  However, the result 'Map' is
-- potentially empty, since the original map might have contained just
-- a single item.
--
-- > maxView (fromList ((5,"a") :| [(3,"b")])) == ("a", Data.Map.singleton 3 "b")
maxView :: NEMap k a -> (a, Map k a)
maxView = first snd . deleteFindMax
{-# INLINE maxView #-}

-- | /O(log n)/. Delete and find the minimal key-value pair.
--
-- Note that unlike @Data.Map.deleteFindMax@ for 'Map', this cannot ever
-- fail, and so is a total function. However, the result 'Map' is
-- potentially empty, since the original map might have contained just
-- a single item.
--
-- > deleteFindMax (fromList ((5,"a") :| [(3,"b"), (10,"c")])) == ((10,"c"), Data.Map.fromList [(3,"b"), (5,"a")])
deleteFindMax :: NEMap k a -> ((k, a), Map k a)
deleteFindMax (NEMap k v m) = maybe ((k, v), M.empty) (second (insertMinMap k v))
                            . M.maxViewWithKey
                            $ m
{-# INLINE deleteFindMax #-}

-- | Special property of non-empty maps: The type of non-empty maps over
-- uninhabited keys is itself uninhabited.
--
-- This property also exists for /values/ inside a non-empty container
-- (like for 'NESet', 'NESeq', and 'NEIntMap'); this can be witnessed using
-- the function @'absurd' . 'fold1'@.
--
-- @since 0.3.1.0
absurdNEMap :: NEMap Void a -> b
absurdNEMap = \case {}

-- ---------------------------
-- Combining functions
-- ---------------------------
--
-- Code comes from "Data.Map.Internal" from containers, modified slightly
-- to work with NonEmpty
--
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008

combineEq :: Eq a => NonEmpty (a, b) -> NonEmpty (a, b)
combineEq = \case
    x :| []       -> x :| []
    x :| xx@(_:_) -> go x xx
  where
    go z [] = z :| []
    go z@(kz,_) (x@(kx,xx):xs')
      | kx==kz    = go (kx,xx) xs'
      | otherwise = z NE.<| go x xs'

combineEqWith
    :: Eq a
    => (a -> b -> b -> b)
    -> NonEmpty (a, b)
    -> NonEmpty (a, b)
combineEqWith f = \case
    x :| []       -> x :| []
    x :| xx@(_:_) -> go x xx
  where
    go z [] = z :| []
    go z@(kz,zz) (x@(kx,xx):xs')
      | kx==kz    = let yy = f kx xx zz in go (kx,yy) xs'
      | otherwise = z NE.<| go x xs'
