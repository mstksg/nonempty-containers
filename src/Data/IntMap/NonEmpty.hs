{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE ViewPatterns    #-}

-- |
-- Module      : Data.IntMap.NonEmpty
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- = Non-Empty Finite Integer-Indexed Maps (lazy interface)
--
-- The @'NEIntMap' v@ type represents a non-empty finite map (sometimes
-- called a dictionary) from integer keys to values of type @v@.
-- An 'NEIntMap' is strict in its keys but lazy in its values.
--
-- See documentation for 'NEIntMap' for information on how to convert and
-- manipulate such non-empty maps.
--
-- This module essentially re-imports the API of "Data.IntMap.Lazy" and its
-- 'IntMap' type, along with semantics and asymptotics.  In most
-- situations, asymptotics are different only by a constant factor.  In
-- some situations, asmyptotics are even better (constant-time instead of
-- log-time).
--
-- Because 'NEIntMap' is implemented using 'IntMap', all of the caveats of using
-- 'IntMap' apply (such as the limitation of the maximum size of maps).
--
-- All functions take non-empty maps as inputs.  In situations where their
-- results can be guarunteed to also be non-empty, they also return
-- non-empty maps.  In situations where their results could potentially be
-- empty, 'IntMap' is returned instead.
--
-- Some variants of functions (like 'alter'', 'alterF'', 'adjustMin',
-- 'adjustMax', 'adjustMinWithKey', 'adjustMaxWithKey') are provided in
-- a way restructured to preserve guaruntees of non-empty maps being
-- returned.
--
-- Some functions (like 'mapEither', 'partition', 'split')
-- have modified return types to account for possible configurations of
-- non-emptiness.
--
-- This module is intended to be imported qualified, to avoid name clashes with
-- "Prelude" and "Data.IntMap" functions:
--
-- > import qualified Data.IntMap.NonEmpty as NEIM
--
-- Note that all asmyptotics /O(f(n))/ in this module are actually
-- /O(min(W, f(n)))/, where @W@ is the number of bits in an 'Int' (32 or
-- 64).  That is, if @f(n)@ is greater than @W@, all operations are
-- constant-time.
--
-- At the moment, this package does not provide a variant strict on values
-- for these functions, like /containers/ does.  This is a planned future
-- implementation (PR's are appreciated).  For now, you can simulate
-- a strict interface by manually forcing values before returning results.
module Data.IntMap.NonEmpty (
  -- * Non-Empty IntMap Type
    NEIntMap
  , Key

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

  -- -- ** Universal combining function
  -- , mergeWithKey

  -- * Traversal
  -- ** Map
  , map
  , mapWithKey
  , traverseWithKey1
  , traverseWithKey
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
import           Data.Functor.Identity
import           Data.IntMap.Internal          (IntMap(..))
import           Data.IntMap.NonEmpty.Internal
import           Data.IntSet                   (IntSet)
import           Data.IntSet.NonEmpty.Internal (NEIntSet(..))
import           Data.List.NonEmpty            (NonEmpty(..))
import           Data.Maybe hiding             (mapMaybe)
import           Data.Semigroup.Foldable       (Foldable1)
import           Data.These
import           Prelude hiding                (Foldable(..), map, filter, lookup)
import qualified Data.Foldable                 as F
import qualified Data.IntMap                   as M
import qualified Data.IntSet                   as S
import qualified Data.List.NonEmpty            as NE
import qualified Data.Maybe                    as Maybe
import qualified Data.Semigroup.Foldable       as F1

-- | /O(1)/ match, /O(log n)/ usage of contents. The 'IsNonEmpty' and
-- 'IsEmpty' patterns allow you to treat a 'IntMap' as if it were either
-- a @'IsNonEmpty' n@ (where @n@ is a 'NEIntMap') or an 'IsEmpty'.
--
-- For example, you can pattern match on a 'IntMap':
--
-- @
-- myFunc :: 'IntMap' K X -> Y
-- myFunc ('IsNonEmpty' n) =  -- here, the user provided a non-empty map, and @n@ is the 'NEIntMap'
-- myFunc 'IsEmpty'        =  -- here, the user provided an empty map.
-- @
--
-- Matching on @'IsNonEmpty' n@ means that the original 'IntMap' was /not/
-- empty, and you have a verified-non-empty 'NEIntMap' @n@ to use.
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
-- a 'NEIntMap' back into a 'IntMap', obscuring its non-emptiness (see 'toMap').
pattern IsNonEmpty :: NEIntMap a -> IntMap a
pattern IsNonEmpty n <- (nonEmptyMap->Just n)
  where
    IsNonEmpty n = toMap n

-- | /O(1)/. The 'IsNonEmpty' and 'IsEmpty' patterns allow you to treat
-- a 'IntMap' as if it were either a @'IsNonEmpty' n@ (where @n@ is
-- a 'NEIntMap') or an 'IsEmpty'.
--
-- Matching on 'IsEmpty' means that the original 'IntMap' was empty.
--
-- A case statement handling both 'IsNonEmpty' and 'IsEmpty' provides
-- complete coverage.
--
-- This is a bidirectional pattern, so you can use 'IsEmpty' as an
-- expression, and it will be interpreted as 'Data.IntMap.empty'.
--
-- See 'IsNonEmpty' for more information.
pattern IsEmpty :: IntMap a
pattern IsEmpty <- (M.null->True)
  where
    IsEmpty = M.empty

{-# COMPLETE IsNonEmpty, IsEmpty #-}

-- | /O(log n)/. Unsafe version of 'nonEmptyMap'.  Coerces a 'IntMap' into an
-- 'NEIntMap', but is undefined (throws a runtime exception when evaluation is
-- attempted) for an empty 'IntMap'.
unsafeFromMap
    :: IntMap a
    -> NEIntMap a
unsafeFromMap = withNonEmpty e id
  where
    e = errorWithoutStackTrace "NEIntMap.unsafeFromMap: empty map"
{-# INLINE unsafeFromMap #-}

-- | /O(log n)/. Convert a 'IntMap' into an 'NEIntMap' by adding a key-value
-- pair.  Because of this, we know that the map must have at least one
-- element, and so therefore cannot be empty. If key is already present,
-- will overwrite the original value.
--
-- See 'insertMapMin' for a version that is constant-time if the new key is
-- /strictly smaller than/ all keys in the original map.
--
-- > insertMap 4 "c" (Data.IntMap.fromList [(5,"a"), (3,"b")]) == fromList ((3,"b") :| [(4,"c"), (5,"a")])
-- > insertMap 4 "c" Data.IntMap.empty == singleton 4 "c"
insertMap :: Key -> a -> IntMap a -> NEIntMap a
insertMap k v = withNonEmpty (singleton k v) (insert k v)
{-# INLINE insertMap #-}

-- | /O(log n)/. Convert a 'IntMap' into an 'NEIntMap' by adding a key-value
-- pair.  Because of this, we know that the map must have at least one
-- element, and so therefore cannot be empty. Uses a combining function
-- with the new value as the first argument if the key is already present.
--
-- > insertMapWith (++) 4 "c" (Data.IntMap.fromList [(5,"a"), (3,"b")]) == fromList ((3,"b") :| [(4,"c"), (5,"a")])
-- > insertMapWith (++) 5 "c" (Data.IntMap.fromList [(5,"a"), (3,"b")]) == fromList ((3,"b") :| [(5,"ca")])
insertMapWith
    :: (a -> a -> a)
    -> Key
    -> a
    -> IntMap a
    -> NEIntMap a
insertMapWith f k v = withNonEmpty (singleton k v) (insertWith f k v)
{-# INLINE insertMapWith #-}

-- | /O(log n)/. Convert a 'IntMap' into an 'NEIntMap' by adding a key-value
-- pair.  Because of this, we know that the map must have at least one
-- element, and so therefore cannot be empty. Uses a combining function
-- with the key and new value as the first and second arguments if the key
-- is already present.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (Data.IntMap.fromList [(5,"a"), (3,"b")]) == fromList ((3, "b") :| [(5, "5:xxx|a")])
-- > insertWithKey f 7 "xxx" (Data.IntMap.fromList [(5,"a"), (3,"b")]) == fromList ((3, "b") :| [(5, "a"), (7, "xxx")])
-- > insertWithKey f 5 "xxx" Data.IntMap.empty                         == singleton 5 "xxx"
insertMapWithKey
    :: (Key -> a -> a -> a)
    -> Key
    -> a
    -> IntMap a
    -> NEIntMap a
insertMapWithKey f k v = withNonEmpty (singleton k v) (insertWithKey f k v)
{-# INLINE insertMapWithKey #-}

-- | /O(1)/ Convert a 'IntMap' into an 'NEIntMap' by adding a key-value pair
-- where the key is /strictly less than/ all keys in the input map.  The
-- keys in the original map must all be /strictly greater than/ the new
-- key.  /The precondition is not checked./
--
-- > insertMapMin 2 "c" (Data.IntMap.fromList [(5,"a"), (3,"b")]) == fromList ((2,"c") :| [(3,"b"), (5,"a")])
-- > valid (insertMapMin 2 "c" (Data.IntMap.fromList [(5,"a"), (3,"b")])) == True
-- > valid (insertMapMin 7 "c" (Data.IntMap.fromList [(5,"a"), (3,"b")])) == False
-- > valid (insertMapMin 3 "c" (Data.IntMap.fromList [(5,"a"), (3,"b")])) == False
insertMapMin
    :: Key
    -> a
    -> IntMap a
    -> NEIntMap a
insertMapMin = NEIntMap
{-# INLINE insertMapMin #-}

-- | /O(log n)/ Convert a 'IntMap' into an 'NEIntMap' by adding a key-value pair
-- where the key is /strictly greater than/ all keys in the input map.  The
-- keys in the original map must all be /strictly less than/ the new
-- key.  /The precondition is not checked./
--
-- At the current moment, this is identical simply 'insertMap'; however,
-- it is left both for consistency and as a placeholder for a future
-- version where optimizations are implemented to allow for a faster
-- implementation.
--
-- > insertMap 7 "c" (Data.IntMap.fromList [(5,"a"), (3,"b")]) == fromList ((3,"b") :| [(5,"a"), (7,"c")])

-- these currently are all valid, but shouldn't be
-- > valid (insertMap 7 "c" (Data.IntMap.fromList [(5,"a"), (3,"b")])) == True
-- > valid (insertMap 2 "c" (Data.IntMap.fromList [(5,"a"), (3,"b")])) == False
-- > valid (insertMap 5 "c" (Data.IntMap.fromList [(5,"a"), (3,"b")])) == False
insertMapMax
    :: Key
    -> a
    -> IntMap a
    -> NEIntMap a
insertMapMax k v = withNonEmpty (singleton k v) go
  where
    go (NEIntMap k0 v0 m0) = NEIntMap k0 v0 . insertMaxMap k v $ m0
{-# INLINE insertMapMax #-}

-- | /O(n)/. Build a non-empty map from a non-empty set of keys and
-- a function which for each key computes its value.
--
-- > fromSet (\k -> replicate k 'a') (Data.Set.NonEmpty.fromList (3 :| [5])) == fromList ((5,"aaaaa") :| [(3,"aaa")])
fromSet
    :: (Key -> a)
    -> NEIntSet
    -> NEIntMap a
fromSet f (NEIntSet k ks) = NEIntMap k (f k) (M.fromSet f ks)
{-# INLINE fromSet #-}

-- | /O(n*log n)/. Build a map from a non-empty list of key\/value pairs
-- with a combining function. See also 'fromAscListWith'.
--
-- > fromListWith (++) ((5,"a") :| [(5,"b"), (3,"b"), (3,"a"), (5,"a")]) == fromList ((3, "ab") :| [(5, "aba")])
fromListWith
    :: (a -> a -> a)
    -> NonEmpty (Key, a)
    -> NEIntMap a
fromListWith f = fromListWithKey (const f)
{-# INLINE fromListWith #-}

-- | /O(n*log n)/. Build a map from a non-empty list of key\/value pairs
-- with a combining function. See also 'fromAscListWithKey'.
--
-- > let f k a1 a2 = (show k) ++ a1 ++ a2
-- > fromListWithKey f ((5,"a") :| [(5,"b"), (3,"b"), (3,"a"), (5,"a")]) == fromList ((3, "3ab") :| [(5, "5a5ba")])
fromListWithKey
    :: (Key -> a -> a -> a)
    -> NonEmpty (Key, a)
    -> NEIntMap a
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
    :: NonEmpty (Key, a)
    -> NEIntMap a
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
    :: (a -> a -> a)
    -> NonEmpty (Key, a)
    -> NEIntMap a
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
    :: (Key -> a -> a -> a)
    -> NonEmpty (Key, a)
    -> NEIntMap a
fromAscListWithKey f = fromDistinctAscList . combineEqWith f
{-# INLINE fromAscListWithKey #-}

-- | /O(n)/. Build a map from an ascending non-empty list of distinct
-- elements in linear time. /The precondition is not checked./
--
-- > fromDistinctAscList ((3,"b") :| [(5,"a")]) == fromList ((3, "b") :| [(5, "a")])
-- > valid (fromDistinctAscList ((3,"b") :| [(5,"a")]))          == True
-- > valid (fromDistinctAscList ((3,"b") :| [(5,"a"), (5,"b")])) == False
fromDistinctAscList :: NonEmpty (Key, a) -> NEIntMap a
fromDistinctAscList ((k, v) :| xs) = insertMapMin k v
                                   . M.fromDistinctAscList
                                   $ xs
{-# INLINE fromDistinctAscList #-}

-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
--
-- See 'insertMap' for a version where the first argument is a 'IntMap'.
--
-- > insert 5 'x' (fromList ((5,'a') :| [(3,'b')])) == fromList ((3, 'b') :| [(5, 'x')])
-- > insert 7 'x' (fromList ((5,'a') :| [(3,'b')])) == fromList ((3, 'b') :| [(5, 'a'), (7, 'x')])
insert
    :: Key
    -> a
    -> NEIntMap a
    -> NEIntMap a
insert k v n@(NEIntMap k0 v0 m) = case compare k k0 of
    LT -> NEIntMap k  v  . toMap        $ n
    EQ -> NEIntMap k  v  m
    GT -> NEIntMap k0 v0 . M.insert k v $ m
{-# INLINE insert #-}

-- | /O(log n)/. Insert with a function, combining key, new value and old
-- value. @'insertWithKey' f key value mp@ will insert the pair (key,
-- value) into @mp@ if key does not exist in the map. If the key does
-- exist, the function will insert the pair @(key,f key new_value
-- old_value)@. Note that the key passed to f is the same key passed to
-- 'insertWithKey'.
--
-- See 'insertMapWithKey' for a version where the first argument is a 'IntMap'.
--
-- > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
-- > insertWithKey f 5 "xxx" (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "5:xxx|a")])
-- > insertWithKey f 7 "xxx" (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "a"), (7, "xxx")])
insertWithKey
    :: (Key -> a -> a -> a)
    -> Key
    -> a
    -> NEIntMap a
    -> NEIntMap a
insertWithKey f k v n@(NEIntMap k0 v0 m) = case compare k k0 of
    LT -> NEIntMap k  v          . toMap               $ n
    EQ -> NEIntMap k  (f k v v0) m
    GT -> NEIntMap k0 v0         $ M.insertWithKey f k v m
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
    :: (Key -> a -> a -> a)
    -> Key
    -> a
    -> NEIntMap a
    -> (Maybe a, NEIntMap a)
insertLookupWithKey f k v n@(NEIntMap k0 v0 m) = case compare k k0 of
    LT -> (Nothing, NEIntMap k  v . toMap $ n )
    EQ -> (Just v , NEIntMap k  (f k v v0)  m )
    GT -> NEIntMap k0 v0 <$> M.insertLookupWithKey f k v m
{-# INLINE insertLookupWithKey #-}

-- | /O(log n)/. Delete a key and its value from the non-empty map.
-- A potentially empty map ('IntMap') is returned, since this might delete the
-- last item in the 'NEIntMap'.  When the key is not a member of the map, is
-- equivalent to 'toMap'.
--
-- > delete 5 (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.singleton 3 "b"
-- > delete 7 (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.Singleton [(3, "b"), (5, "a")]
delete :: Key -> NEIntMap a -> IntMap a
delete k n@(NEIntMap k0 v m) = case compare k k0 of
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
    :: (a -> a)
    -> Key
    -> NEIntMap a
    -> NEIntMap a
adjust f = adjustWithKey (const f)
{-# INLINE adjust #-}

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
--
-- > let f key x = (show key) ++ ":new " ++ x
-- > adjustWithKey f 5 (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "5:new a")])
-- > adjustWithKey f 7 (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "b") :| [(5, "a")])
adjustWithKey
    :: (Key -> a -> a)
    -> Key
    -> NEIntMap a
    -> NEIntMap a
adjustWithKey f k n@(NEIntMap k0 v m) = case compare k k0 of
    LT -> n
    EQ -> NEIntMap k0 (f k0 v) m
    GT -> NEIntMap k0 v . M.adjustWithKey f k $ m
{-# INLINE adjustWithKey #-}

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
--
-- Returns a potentially empty map ('IntMap'), because we can't know ahead of
-- time if the function returns 'Nothing' and deletes the final item in the
-- 'NEIntMap'.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > update f 5 (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.fromList [(3, "b"), (5, "new a")]
-- > update f 7 (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.fromList [(3, "b"), (5, "a")]
-- > update f 3 (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.singleton 5 "a"
update
    :: (a -> Maybe a)
    -> Key
    -> NEIntMap a
    -> IntMap a
update f = updateWithKey (const f)
{-# INLINE update #-}

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
--
-- Returns a potentially empty map ('IntMap'), because we can't know ahead of
-- time if the function returns 'Nothing' and deletes the final item in the
-- 'NEIntMap'.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateWithKey f 5 (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.fromList [(3, "b"), (5, "5:new a")]
-- > updateWithKey f 7 (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.fromList [(3, "b"), (5, "a")]
-- > updateWithKey f 3 (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.singleton 5 "a"
updateWithKey
    :: (Key -> a -> Maybe a)
    -> Key
    -> NEIntMap a
    -> IntMap a
updateWithKey f k n@(NEIntMap k0 v m) = case compare k k0 of
    LT -> toMap n
    EQ -> maybe m (flip (insertMinMap k0) m) . f k0 $ v
    GT -> insertMinMap k0 v . M.updateWithKey f k   $ m
{-# INLINE updateWithKey #-}

-- | /O(min(n,W))/. Lookup and update.
-- The function returns original value, if it is updated.
-- This is different behavior than @Data.Map.NonEmpty.updateLookupWithKey@.
-- Returns the original key value if the map entry is deleted.
--
-- Returns a potentially empty map ('IntMap') in the case that we delete
-- the final key of a singleton map.
--
-- > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
-- > updateLookupWithKey f 5 (fromList ((5,"a") :| [(3,"b")])) == (Just "5:new a", Data.IntMap.fromList ((3, "b") :| [(5, "5:new a")]))
-- > updateLookupWithKey f 7 (fromList ((5,"a") :| [(3,"b")])) == (Nothing,  Data.IntMap.fromList ((3, "b") :| [(5, "a")]))
-- > updateLookupWithKey f 3 (fromList ((5,"a") :| [(3,"b")])) == (Just "b", Data.IntMap.singleton 5 "a")
updateLookupWithKey
    :: (Key -> a -> Maybe a)
    -> Key
    -> NEIntMap a
    -> (Maybe a, IntMap a)
updateLookupWithKey f k n@(NEIntMap k0 v m) = case compare k k0 of
    LT -> (Nothing, toMap n)
    EQ -> let u = f k0 v
          in  (Just v, maybe m (flip (insertMinMap k0) m) u)
    GT -> fmap (insertMinMap k0 v) . M.updateLookupWithKey f k $ m
{-# INLINE updateLookupWithKey #-}

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at
-- @k@, or absence thereof. 'alter' can be used to insert, delete, or
-- update a value in a 'IntMap'. In short : @Data.IntMap.lookup k ('alter'
-- f k m) = f ('lookup' k m)@.
--
-- Returns a potentially empty map ('IntMap'), because we can't know ahead of
-- time if the function returns 'Nothing' and deletes the final item in the
-- 'NEIntMap'.
--
-- See 'alterF'' for a version that disallows deletion, and so therefore
-- can return 'NEIntMap'.
--
-- > let f _ = Nothing
-- > alter f 7 (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.fromList [(3, "b"), (5, "a")]
-- > alter f 5 (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.singleton 3 "b"
-- >
-- > let f _ = Just "c"
-- > alter f 7 (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.fromList [(3, "b"), (5, "a"), (7, "c")]
-- > alter f 5 (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.fromList [(3, "b"), (5, "c")]
alter
    :: (Maybe a -> Maybe a)
    -> Key
    -> NEIntMap a
    -> IntMap a
alter f k n@(NEIntMap k0 v m) = case compare k k0 of
    LT -> ($ toMap n) . maybe id (insertMinMap k ) $ f Nothing
    EQ -> ($ m      ) . maybe id (insertMinMap k0) $ f (Just v)
    GT -> insertMinMap k0 v . M.alter f k $ m
{-# INLINE alter #-}

-- | /O(log n)/. The expression (@'alterF' f k map@) alters the value @x@
-- at @k@, or absence thereof.  'alterF' can be used to inspect, insert,
-- delete, or update a value in a 'IntMap'.  In short: @Data.IntMap.lookup
-- k \<$\> 'alterF' f k m = f ('lookup' k m)@.
--
-- Example:
--
-- @
-- interactiveAlter :: Int -> NEIntMap Int String -> IO (IntMap Int String)
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
-- Like @Data.IntMap.alterF@ for 'IntMap', 'alterF' can be considered
-- to be a unifying generalization of 'lookup' and 'delete'; however, as
-- a constrast, it cannot be used to implement 'insert', because it must
-- return a 'IntMap' instead of an 'NEIntMap' (because the function might delete
-- the final item in the 'NEIntMap').  When used with trivial functors like
-- 'Identity' and 'Const', it is often slightly slower than
-- specialized 'lookup' and 'delete'. However, when the functor is
-- non-trivial and key comparison is not particularly cheap, it is the
-- fastest way.
--
-- See 'alterF'' for a version that disallows deletion, and so therefore
-- can return 'NEIntMap' and be used to implement 'insert'
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
-- Note: Unlike @Data.IntMap.alterF@ for 'IntMap', 'alterF' is /not/ a flipped
-- version of the 'Control.Lens.At.at' combinator from "Control.Lens.At".
-- However, it match the shape expected from most functions expecting
-- lenses, getters, and setters, so can be thought of as a "psuedo-lens",
-- with virtually the same practical applications as a legitimate lens.
alterF
    :: Functor f
    => (Maybe a -> f (Maybe a))
    -> Key
    -> NEIntMap a
    -> f (IntMap a)
alterF f k n@(NEIntMap k0 v m) = case compare k k0 of
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
-- guarantee that the result is also a non-empty IntMap.
alter'
    :: (Maybe a -> a)
    -> Key
    -> NEIntMap a
    -> NEIntMap a
alter' f k n@(NEIntMap k0 v m) = case compare k k0 of
    LT -> NEIntMap k  (f Nothing) . toMap      $ n
    EQ -> NEIntMap k0 (f (Just v))             $ m
    GT -> NEIntMap k0 v . M.alter (Just . f) k $ m
{-# INLINE alter' #-}

-- | /O(log n)/. Variant of 'alterF' that disallows deletion.  Allows us to
-- guarantee that the result is also a non-empty IntMap.
--
-- Like @Data.IntMap.alterF@ for 'IntMap', can be used to generalize and unify
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
-- undefined behavior for "Data.IntMap".  @Data.IntMap.alterF@ will actually
-- /maintain/ the original key in the map when used with 'Identity';
-- however, @Data.IntMap.insertWith@ will /replace/ the orginal key in the
-- map.  The rewrite rule for 'alterF'' has chosen to be faithful to
-- @Data.IntMap.insertWith@, and /not/ @Data.IntMap.alterF@, for the sake of
-- a cleaner implementation.
alterF'
    :: Functor f
    => (Maybe a -> f a)
    -> Key
    -> NEIntMap a
    -> f (NEIntMap a)
alterF' f k n@(NEIntMap k0 v m) = case compare k k0 of
    LT -> flip (NEIntMap k ) (toMap n) <$> f Nothing
    EQ -> flip (NEIntMap k0) m         <$> f (Just v)
    GT -> NEIntMap k0 v <$> M.alterF (fmap Just . f) k m
{-# INLINABLE [2] alterF' #-}

-- if f ~ Const b, it's a lookup
{-# RULES
"alterF'/Const" forall k (f :: Maybe a -> Const b a) . alterF' f k = \m -> Const . getConst . f $ lookup k m
 #-}
-- if f ~ Identity, it's an insertWith
{-# RULES
"alterF'/Identity" forall k (f :: Maybe a -> Identity a) . alterF' f k = Identity . insertWith (\_ -> runIdentity . f . Just) k (runIdentity (f Nothing))
 #-}

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
    :: Key
    -> NEIntMap a
    -> Maybe a
lookup k (NEIntMap k0 v m) = case compare k k0 of
    LT -> Nothing
    EQ -> Just v
    GT -> M.lookup k m
{-# INLINE lookup #-}

-- | /O(log n)/. Find the value at a key. Returns 'Nothing' when the
-- element can not be found.
--
-- prop> fromList ((5, 'a') :| [(3, 'b')]) !? 1 == Nothing
-- prop> fromList ((5, 'a') :| [(3, 'b')]) !? 5 == Just 'a'
(!?) :: NEIntMap a -> Key -> Maybe a
(!?) = flip lookup
{-# INLINE (!?) #-}

-- | /O(log n)/. Find the value at a key. Calls 'error' when the element
-- can not be found.
--
-- > fromList ((5,'a') :| [(3,'b')]) ! 1    Error: element not in the map
-- > fromList ((5,'a') :| [(3,'b')]) ! 5 == 'a'
(!) :: NEIntMap a -> Key -> a
(!) m k = fromMaybe e $ m !? k
  where
    e = error "NEIntMap.!: given key is not an element in the map"
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
    :: a
    -> Key
    -> NEIntMap a
    -> a
findWithDefault def k (NEIntMap k0 v m) = case compare k k0 of
    LT -> def
    EQ -> v
    GT -> M.findWithDefault def k m
{-# INLINE findWithDefault #-}

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
--
-- > member 5 (fromList ((5,'a') :| [(3,'b')])) == True
-- > member 1 (fromList ((5,'a') :| [(3,'b')])) == False
member :: Key -> NEIntMap a -> Bool
member k (NEIntMap k0 _ m) = case compare k k0 of
    LT -> False
    EQ -> True
    GT -> M.member k m
{-# INLINE member #-}

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
--
-- > notMember 5 (fromList ((5,'a') :| [(3,'b')])) == False
-- > notMember 1 (fromList ((5,'a') :| [(3,'b')])) == True
notMember :: Key -> NEIntMap a -> Bool
notMember k (NEIntMap k0 _ m) = case compare k k0 of
    LT -> True
    EQ -> False
    GT -> M.notMember k m
{-# INLINE notMember #-}

-- | /O(log n)/. Find largest key smaller than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupLT 3 (fromList ((3,'a') :| [(5,'b')])) == Nothing
-- > lookupLT 4 (fromList ((3,'a') :| [(5,'b')])) == Just (3, 'a')
lookupLT :: Key -> NEIntMap a -> Maybe (Key, a)
lookupLT k (NEIntMap k0 v m) = case compare k k0 of
    LT -> Nothing
    EQ -> Nothing
    GT -> M.lookupLT k m <|> Just (k0, v)
{-# INLINE lookupLT #-}

-- | /O(log n)/. Find smallest key greater than the given one and return the
-- corresponding (key, value) pair.
--
-- > lookupGT 4 (fromList ((3,'a') :| [(5,'b')])) == Just (5, 'b')
-- > lookupGT 5 (fromList ((3,'a') :| [(5,'b')])) == Nothing
lookupGT :: Key -> NEIntMap a -> Maybe (Key, a)
lookupGT k (NEIntMap k0 v m) = case compare k k0 of
    LT -> Just (k0, v)
    EQ -> lookupMinMap m
    GT -> M.lookupGT k m
{-# INLINE lookupGT #-}

-- | /O(log n)/. Find largest key smaller or equal to the given one and return
-- the corresponding (key, value) pair.
--
-- > lookupLE 2 (fromList ((3,'a') :| [(5,'b')])) == Nothing
-- > lookupLE 4 (fromList ((3,'a') :| [(5,'b')])) == Just (3, 'a')
-- > lookupLE 5 (fromList ((3,'a') :| [(5,'b')])) == Just (5, 'b')
lookupLE :: Key -> NEIntMap a -> Maybe (Key, a)
lookupLE k (NEIntMap k0 v m) = case compare k k0 of
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
lookupGE :: Key -> NEIntMap a -> Maybe (Key, a)
lookupGE k (NEIntMap k0 v m) = case compare k k0 of
    LT -> Just (k0, v)
    EQ -> Just (k0, v)
    GT -> M.lookupGE k m
{-# INLINE lookupGE #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Union with a combining function.
--
-- > unionWith (++) (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == fromList ((3, "b") :| [(5, "aA"), (7, "C")])
unionWith
    :: (a -> a -> a)
    -> NEIntMap a
    -> NEIntMap a
    -> NEIntMap a
unionWith f n1@(NEIntMap k1 v1 m1) n2@(NEIntMap k2 v2 m2) = case compare k1 k2 of
    LT -> NEIntMap k1 v1        . M.unionWith f m1 . toMap $ n2
    EQ -> NEIntMap k1 (f v1 v2) . M.unionWith f m1         $ m2
    GT -> NEIntMap k2 v2        . M.unionWith f (toMap n1) $ m2
{-# INLINE unionWith #-}

-- | /O(m*log(n\/m + 1)), m <= n/.
-- Union with a combining function, given the matching key.
--
-- > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
-- > unionWithKey f (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == fromList ((3, "b") :| [(5, "5:a|A"), (7, "C")])
unionWithKey
    :: (Key -> a -> a -> a)
    -> NEIntMap a
    -> NEIntMap a
    -> NEIntMap a
unionWithKey f n1@(NEIntMap k1 v1 m1) n2@(NEIntMap k2 v2 m2) = case compare k1 k2 of
    LT -> NEIntMap k1 v1           . M.unionWithKey f m1 . toMap $ n2
    EQ -> NEIntMap k1 (f k1 v1 v2) . M.unionWithKey f m1         $ m2
    GT -> NEIntMap k2 v2           . M.unionWithKey f (toMap n1) $ m2
{-# INLINE unionWithKey #-}

-- | The union of a non-empty list of maps, with a combining operation:
--   (@'unionsWith' f == 'Data.Foldable.foldl1' ('unionWith' f)@).
--
-- > unionsWith (++) (fromList ((5, "a") :| [(3, "b")]) :| [fromList ((5, "A") :| [(7, "C")]), fromList ((5, "A3") :| [(3, "B3")])])
-- >     == fromList ((3, "bB3") :| [(5, "aAA3"), (7, "C")])
unionsWith
    :: Foldable1 f
    => (a -> a -> a)
    -> f (NEIntMap a)
    -> NEIntMap a
unionsWith f (F1.toNonEmpty->(m :| ms)) = F.foldl' (unionWith f) m ms
{-# INLINE unionsWith #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Difference of two maps.
-- Return elements of the first map not existing in the second map.
--
-- Returns a potentially empty map ('IntMap'), in case the first map is
-- a subset of the second map.
--
-- > difference (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == Data.IntMap.singleton 3 "b"
difference
    :: NEIntMap a
    -> NEIntMap b
    -> IntMap a
difference n1@(NEIntMap k1 v1 m1) n2@(NEIntMap k2 _ m2) = case compare k1 k2 of
    -- k1 is not in n2, so cannot be deleted
    LT -> insertMinMap k1 v1 $ m1 `M.difference` toMap n2
    -- k2 deletes k1, and only k1
    EQ -> m1 `M.difference` m2
    -- k2 is not in n1, so cannot delete anything, so we can just difference n1 // m2.
    GT -> toMap n1 `M.difference` m2
{-# INLINE difference #-}

-- | Same as 'difference'.
(\\)
    :: NEIntMap a
    -> NEIntMap b
    -> IntMap a
(\\) = difference
{-# INLINE (\\) #-}

-- | /O(n+m)/. Difference with a combining function.
-- When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- Returns a potentially empty map ('IntMap'), in case the first map is
-- a subset of the second map and the function returns 'Nothing' for every
-- pair.
--
-- > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
-- > differenceWith f (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(3, "B"), (7, "C")]))
-- >     == Data.IntMap.singleton 3 "b:B"
differenceWith
    :: (a -> b -> Maybe a)
    -> NEIntMap a
    -> NEIntMap b
    -> IntMap a
differenceWith f = differenceWithKey (const f)
{-# INLINE differenceWith #-}

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- Returns a potentially empty map ('IntMap'), in case the first map is
-- a subset of the second map and the function returns 'Nothing' for every
-- pair.
--
-- > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
-- > differenceWithKey f (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(3, "B"), (10, "C")]))
-- >     == Data.IntMap.singleton 3 "3:b|B"
differenceWithKey
    :: (Key -> a -> b -> Maybe a)
    -> NEIntMap a
    -> NEIntMap b
    -> IntMap a
differenceWithKey f n1@(NEIntMap k1 v1 m1) n2@(NEIntMap k2 v2 m2) = case compare k1 k2 of
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
-- Returns a potentially empty map ('IntMap'), in case the two maps share no
-- keys in common.
--
-- > intersection (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == Data.IntMap.singleton 5 "a"
intersection
    :: NEIntMap a
    -> NEIntMap b
    -> IntMap a
intersection n1@(NEIntMap k1 v1 m1) n2@(NEIntMap k2 _ m2) = case compare k1 k2 of
    -- k1 is not in n2
    LT -> m1 `M.intersection` toMap n2
    -- k1 and k2 are a part of the result
    EQ -> insertMinMap k1 v1 $ m1 `M.intersection` m2
    -- k2 is not in n1
    GT -> toMap n1 `M.intersection` m2
{-# INLINE intersection #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Intersection with a combining function.
--
-- Returns a potentially empty map ('IntMap'), in case the two maps share no
-- keys in common.
--
-- > intersectionWith (++) (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == Data.IntMap.singleton 5 "aA"
intersectionWith
    :: (a -> b -> c)
    -> NEIntMap a
    -> NEIntMap b
    -> IntMap c
intersectionWith f = intersectionWithKey (const f)
{-# INLINE intersectionWith #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Intersection with a combining function.
--
-- Returns a potentially empty map ('IntMap'), in case the two maps share no
-- keys in common.
--
-- > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
-- > intersectionWithKey f (fromList ((5, "a") :| [(3, "b")])) (fromList ((5, "A") :| [(7, "C")])) == Data.IntMap.singleton 5 "5:a|A"
intersectionWithKey
    :: (Key -> a -> b -> c)
    -> NEIntMap a
    -> NEIntMap b
    -> IntMap c
intersectionWithKey f n1@(NEIntMap k1 v1 m1) n2@(NEIntMap k2 v2 m2) = case compare k1 k2 of
    -- k1 is not in n2
    LT -> M.intersectionWithKey f m1 (toMap n2)
    -- k1 and k2 are a part of the result
    EQ -> insertMinMap k1 (f k1 v1 v2) $ M.intersectionWithKey f m1 m2
    -- k2 is not in n1
    GT -> M.intersectionWithKey f (toMap n1) m2
{-# INLINE intersectionWithKey #-}

-- | /O(n)/. IntMap a function over all values in the map.
--
-- > let f key x = (show key) ++ ":" ++ x
-- > mapWithKey f (fromList ((5,"a") :| [(3,"b")])) == fromList ((3, "3:b") :| [(5, "5:a")])
mapWithKey :: (Key -> a -> b) -> NEIntMap a -> NEIntMap b
mapWithKey f (NEIntMap k v m) = NEIntMap k (f k v) (M.mapWithKey f m)
{-# NOINLINE [1] mapWithKey #-}
{-# RULES
"mapWithKey/mapWithKey" forall f g xs . mapWithKey f (mapWithKey g xs) =
  mapWithKey (\k a -> f k (g k a)) xs
"mapWithKey/map" forall f g xs . mapWithKey f (map g xs) =
  mapWithKey (\k a -> f k (g a)) xs
"map/mapWithKey" forall f g xs . map f (mapWithKey g xs) =
  mapWithKey (\k a -> f (g k a)) xs
 #-}

-- | /O(n)/. The function 'mapAccum' threads an accumulating argument
-- through the map in ascending order of keys.
--
-- > let f a b = (a ++ b, b ++ "X")
-- > mapAccum f "Everything: " (fromList ((5,"a") :| [(3,"b")])) == ("Everything: ba", fromList ((3, "bX") :| [(5, "aX")]))
mapAccum
    :: (a -> b -> (a, c))
    -> a
    -> NEIntMap b
    -> (a, NEIntMap c)
mapAccum f = mapAccumWithKey (\x _ -> f x)
{-# INLINE mapAccum #-}

-- | /O(n)/. The function 'mapAccumWithKey' threads an accumulating
-- argument through the map in ascending order of keys.
--
-- > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
-- > mapAccumWithKey f "Everything:" (fromList ((5,"a") :| [(3,"b")])) == ("Everything: 3-b 5-a", fromList ((3, "bX") :| [(5, "aX")]))
mapAccumWithKey
    :: (a -> Key -> b -> (a, c))
    -> a
    -> NEIntMap b
    -> (a, NEIntMap c)
mapAccumWithKey f z0 (NEIntMap k v m) = (z2, NEIntMap k v' m')
  where
    ~(z1, v') = f z0 k v
    ~(z2, m') = M.mapAccumWithKey f z1 m
{-# INLINE mapAccumWithKey #-}

-- | /O(n)/. The function 'mapAccumRWithKey' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey
    :: (a -> Key -> b -> (a, c))
    -> a
    -> NEIntMap b
    -> (a, NEIntMap c)
mapAccumRWithKey f z0 (NEIntMap k v m) = (z2, NEIntMap k v' m')
  where
    ~(z1, m') = M.mapAccumRWithKey f z0 m
    ~(z2, v') = f z1 k v
{-# INLINE mapAccumRWithKey #-}

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
    :: (Key -> Key)
    -> NEIntMap a
    -> NEIntMap a
mapKeys f (NEIntMap k0 v0 m) = fromListWith const
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
    :: (a -> a -> a)
    -> (Key -> Key)
    -> NEIntMap a
    -> NEIntMap a
mapKeysWith c f (NEIntMap k0 v0 m) = fromListWith c
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
    :: (Key -> Key)
    -> NEIntMap a
    -> NEIntMap a
mapKeysMonotonic f (NEIntMap k v m) = NEIntMap (f k) v
                                 . M.mapKeysMonotonic f
                                 $ m
{-# INLINE mapKeysMonotonic #-}

-- | /O(n)/. Fold the keys and values in the map using the given right-associative
-- binary operator, such that
-- @'foldrWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
--
-- For example,
--
-- > keysList map = foldrWithKey (\k x ks -> k:ks) [] map
foldrWithKey :: (Key -> a -> b -> b) -> b -> NEIntMap a -> b
foldrWithKey f z (NEIntMap k v m) = f k v . M.foldrWithKey f z $ m
{-# INLINE foldrWithKey #-}

-- | /O(n)/. Fold the keys and values in the map using the given left-associative
-- binary operator, such that
-- @'foldlWithKey' f z == 'Prelude.foldl' (\\z' (kx, x) -> f z' kx x) z . 'toAscList'@.
--
-- For example,
--
-- > keysList = reverse . foldlWithKey (\ks k x -> k:ks) []
foldlWithKey :: (a -> Key -> b -> a) -> a -> NEIntMap b -> a
foldlWithKey f z (NEIntMap k v m) = M.foldlWithKey f (f z k v) m
{-# INLINE foldlWithKey #-}

-- | /O(n)/. A strict version of 'foldr1'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldr1' :: (a -> a -> a) -> NEIntMap a -> a
foldr1' f (NEIntMap _ v m) = case M.maxView m of
    Nothing      -> v
    Just (y, m') -> let !z = M.foldr' f y m' in v `f` z
{-# INLINE foldr1' #-}

-- | /O(n)/. A strict version of 'foldl1'. Each application of the operator
-- is evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldl1' :: (a -> a -> a) -> NEIntMap a -> a
foldl1' f (NEIntMap _ v m) = M.foldl' f v m
{-# INLINE foldl1' #-}

-- | /O(n)/. A strict version of 'foldrWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldrWithKey' :: (Key -> a -> b -> b) -> b -> NEIntMap a -> b
foldrWithKey' f z (NEIntMap k v m) = f k v y
  where
    !y = M.foldrWithKey f z m
{-# INLINE foldrWithKey' #-}

-- | /O(n)/. A strict version of 'foldlWithKey'. Each application of the operator is
-- evaluated before using the result in the next application. This
-- function is strict in the starting value.
foldlWithKey' :: (a -> Key -> b -> a) -> a -> NEIntMap b -> a
foldlWithKey' f z (NEIntMap k v m) = M.foldlWithKey' f x m
  where
    !x = f z k v
{-# INLINE foldlWithKey' #-}

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList ((5,"a") :| [(3,"b")])) == (3 :| [5])
keys :: NEIntMap a -> NonEmpty Key
keys (NEIntMap k _ m) = k :| M.keys m
{-# INLINE keys #-}

-- | /O(n)/. An alias for 'toAscList'. Return all key\/value pairs in the map
-- in ascending key order.
--
-- > assocs (fromList ((5,"a") :| [(3,"b")])) == ((3,"b") :| [(5,"a")])
assocs :: NEIntMap a -> NonEmpty (Key, a)
assocs = toList
{-# INLINE assocs #-}

-- | /O(n)/. The non-empty set of all keys of the map.
--
-- > keysSet (fromList ((5,"a") :| [(3,"b")])) == Data.Set.NonEmpty.fromList (3 :| [5])
keysSet :: NEIntMap a -> NEIntSet
keysSet (NEIntMap k _ m) = NEIntSet k (M.keysSet m)
{-# INLINE keysSet #-}

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys are
-- in ascending order.
--
-- > toAscList (fromList ((5,"a") :| [(3,"b")])) == ((3,"b") :| [(5,"a")])
toAscList :: NEIntMap a -> NonEmpty (Key, a)
toAscList = toList
{-# INLINE toAscList #-}

-- | /O(n)/. Convert the map to a list of key\/value pairs where the keys
-- are in descending order.
--
-- > toDescList (fromList ((5,"a") :| [(3,"b")])) == ((5,"a") :| [(3,"b")])
toDescList :: NEIntMap a -> NonEmpty (Key, a)
toDescList (NEIntMap k0 v0 m) = M.foldlWithKey' go ((k0, v0) :| []) m
  where
    go xs k v = (k, v) NE.<| xs
{-# INLINE toDescList #-}

-- | /O(n)/. Filter all values that satisfy the predicate.
--
-- Returns a potentially empty map ('IntMap'), because we could
-- potentailly filter out all items in the original 'NEIntMap'.
--
-- > filter (> "a") (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.singleton 3 "b"
-- > filter (> "x") (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.empty
-- > filter (< "a") (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.empty
filter
    :: (a -> Bool)
    -> NEIntMap a
    -> IntMap a
filter f (NEIntMap k v m)
    | f v       = insertMinMap k v . M.filter f $ m
    | otherwise = M.filter f m
{-# INLINE filter #-}

-- | /O(n)/. Filter all keys\/values that satisfy the predicate.
--
-- Returns a potentially empty map ('IntMap'), because we could
-- potentailly filter out all items in the original 'NEIntMap'.
--
-- > filterWithKey (\k _ -> k > 4) (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.singleton 5 "a"
filterWithKey
    :: (Key -> a -> Bool)
    -> NEIntMap a
    -> IntMap a
filterWithKey f (NEIntMap k v m)
    | f k v     = insertMinMap k v . M.filterWithKey f $ m
    | otherwise = M.filterWithKey f m
{-# INLINE filterWithKey #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Restrict an 'NEIntMap' to only those keys
-- found in a 'Data.Set.Set'.
--
-- @
-- m \`restrictKeys\` s = 'filterWithKey' (\k _ -> k ``Set.member`` s) m
-- m \`restrictKeys\` s = m ``intersection`` 'fromSet' (const ()) s
-- @
restrictKeys
    :: NEIntMap a
    -> IntSet
    -> IntMap a
restrictKeys n@(NEIntMap k v m) xs = case S.minView xs of
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
-- an 'NEIntMap'.
--
-- @
-- m \`withoutKeys\` s = 'filterWithKey' (\k _ -> k ``Set.notMember`` s) m
-- m \`withoutKeys\` s = m ``difference`` 'fromSet' (const ()) s
-- @
withoutKeys
    :: NEIntMap a
    -> IntSet
    -> IntMap a
withoutKeys n@(NEIntMap k v m) xs = case S.minView xs of
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
    -> NEIntMap a
    -> These (NEIntMap a) (NEIntMap a)
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
    :: (Key -> a -> Bool)
    -> NEIntMap a
    -> These (NEIntMap a) (NEIntMap a)
partitionWithKey f n@(NEIntMap k v m0) = case (nonEmptyMap m1, nonEmptyMap m2) of
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

-- | /O(n)/. Map values and collect the 'Just' results.
--
-- Returns a potentially empty map ('IntMap'), because the function could
-- potentially return 'Nothing' on all items in the 'NEIntMap'.
--
-- > let f x = if x == "a" then Just "new a" else Nothing
-- > mapMaybe f (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.singleton 5 "new a"
mapMaybe
    :: (a -> Maybe b)
    -> NEIntMap a
    -> IntMap b
mapMaybe f = mapMaybeWithKey (const f)
{-# INLINE mapMaybe #-}

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
--
-- Returns a potentially empty map ('IntMap'), because the function could
-- potentially return 'Nothing' on all items in the 'NEIntMap'.
--
-- > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
-- > mapMaybeWithKey f (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.singleton 3 "key : 3"
mapMaybeWithKey
    :: (Key -> a -> Maybe b)
    -> NEIntMap a
    -> IntMap b
mapMaybeWithKey f (NEIntMap k v m) = ($ M.mapMaybeWithKey f m)
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
    -> NEIntMap a
    -> These (NEIntMap b) (NEIntMap c)
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
    :: (Key -> a -> Either b c)
    -> NEIntMap a
    -> These (NEIntMap b) (NEIntMap c)
mapEitherWithKey f (NEIntMap k v m0) = case (nonEmptyMap m1, nonEmptyMap m2) of
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
-- containing up to two 'NEIntMap's based on splitting the map into maps
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
    :: Key
    -> NEIntMap a
    -> Maybe (These (NEIntMap a) (NEIntMap a))
split k n@(NEIntMap k0 v m0) = case compare k k0 of
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
    :: Key
    -> NEIntMap a
    -> These a (These (NEIntMap a) (NEIntMap a))
splitLookup k n@(NEIntMap k0 v0 m0) = case compare k k0 of
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
    :: NEIntMap a
    -> NonEmpty (NEIntMap a)
splitRoot (NEIntMap k v m) = singleton k v
                       :| Maybe.mapMaybe nonEmptyMap (M.splitRoot m)
{-# INLINE splitRoot #-}

-- | /O(m*log(n\/m + 1)), m <= n/.
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' (==)@).
isSubmapOf :: Eq a => NEIntMap a -> NEIntMap a -> Bool
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
    :: (a -> b -> Bool)
    -> NEIntMap a
    -> NEIntMap b
    -> Bool
isSubmapOfBy f (NEIntMap k v m0) (toMap->m1) = kvSub
                                         && M.isSubmapOfBy f m0 m1
  where
    kvSub = case M.lookup k m1 of
      Just v0 -> f v v0
      Nothing -> False
{-# INLINE isSubmapOfBy #-}

-- | /O(m*log(n\/m + 1)), m <= n/. Is this a proper submap? (ie. a submap
-- but not equal). Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy'
-- (==)@).
isProperSubmapOf :: Eq a => NEIntMap a -> NEIntMap a -> Bool
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
    :: (a -> b -> Bool)
    -> NEIntMap a
    -> NEIntMap b
    -> Bool
isProperSubmapOfBy f m1 m2 = M.size (neimIntMap m1) < M.size (neimIntMap m2)
                          && isSubmapOfBy f m1 m2
{-# INLINE isProperSubmapOfBy #-}

-- | /O(1)/. The minimal key of the map.  Note that this is total, making
-- 'Data.IntMap.lookupMin' obsolete.  It is constant-time, so has better
-- asymptotics than @Data.IntMap.lookupMin@ and @Data.IntMap.findMin@, as well.
--
-- > findMin (fromList ((5,"a") :| [(3,"b")])) == (3,"b")
findMin :: NEIntMap a -> (Key, a)
findMin (NEIntMap k v _) = (k, v)
{-# INLINE findMin #-}

-- | /O(log n)/. The maximal key of the map.  Note that this is total, making
-- 'Data.IntMap.lookupMin' obsolete.
--
-- > findMax (fromList ((5,"a") :| [(3,"b")])) == (5,"a")
findMax :: NEIntMap a -> (Key, a)
findMax (NEIntMap k v m) = fromMaybe (k, v) . lookupMaxMap $ m
{-# INLINE findMax #-}

-- | /O(1)/. Delete the minimal key. Returns a potentially empty map
-- ('IntMap'), because we might end up deleting the final key in a singleton
-- map.  It is constant-time, so has better asymptotics than
-- 'Data.IntMap.deleteMin'.
--
-- > deleteMin (fromList ((5,"a") :| [(3,"b"), (7,"c")])) == Data.IntMap.fromList [(5,"a"), (7,"c")]
-- > deleteMin (singleton 5 "a") == Data.IntMap.empty
deleteMin :: NEIntMap a -> IntMap a
deleteMin (NEIntMap _ _ m) = m
{-# INLINE deleteMin #-}

-- | /O(log n)/. Delete the maximal key. Returns a potentially empty map
-- ('IntMap'), because we might end up deleting the final key in a singleton
-- map.
--
-- > deleteMax (fromList ((5,"a") :| [(3,"b"), (7,"c")])) == Data.IntMap.fromList [(3,"b"), (5,"a")]
-- > deleteMax (singleton 5 "a") == Data.IntMap.empty
deleteMax :: NEIntMap a -> IntMap a
deleteMax (NEIntMap k v m) = case M.maxView m of
    Nothing      -> M.empty
    Just (_, m') -> insertMinMap k v m'
{-# INLINE deleteMax #-}

-- | /O(1)/ if delete, /O(log n)/ otherwise. Update the value at the
-- minimal key.  Returns a potentially empty map ('IntMap'), because we might
-- end up deleting the final key in the map if the function returns
-- 'Nothing'.  See 'adjustMin' for a version that can guaruntee that we
-- return a non-empty map.
--
-- > updateMin (\ a -> Just ("X" ++ a)) (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.fromList [(3, "Xb"), (5, "a")]
-- > updateMin (\ _ -> Nothing)         (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.singleton 5 "a"
updateMin :: (a -> Maybe a) -> NEIntMap a -> IntMap a
updateMin f = updateMinWithKey (const f)
{-# INLINE updateMin #-}

-- | /O(1)/. A version of 'updateMin' that disallows deletion, allowing us
-- to guarantee that the result is also non-empty.
adjustMin :: (a -> a) -> NEIntMap a -> NEIntMap a
adjustMin f = adjustMinWithKey (const f)
{-# INLINE adjustMin #-}

-- | /O(1)/ if delete, /O(log n)/ otherwise. Update the value at the
-- minimal key.  Returns a potentially empty map ('IntMap'), because we might
-- end up deleting the final key in the map if the function returns
-- 'Nothing'.  See 'adjustMinWithKey' for a version that guaruntees
-- a non-empty map.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.singleton 5 "a"
updateMinWithKey :: (Key -> a -> Maybe a) -> NEIntMap a -> IntMap a
updateMinWithKey f (NEIntMap k v m) = ($ m) . maybe id (insertMinMap k) $ f k v
{-# INLINE updateMinWithKey #-}

-- | /O(1)/. A version of 'adjustMaxWithKey' that disallows deletion,
-- allowing us to guarantee that the result is also non-empty.  Note that
-- it also is able to have better asymptotics than 'updateMinWithKey' in
-- general.
adjustMinWithKey :: (Key -> a -> a) -> NEIntMap a -> NEIntMap a
adjustMinWithKey f (NEIntMap k v m) = NEIntMap k (f k v) m
{-# INLINE adjustMinWithKey #-}

-- | /O(log n)/. Update the value at the maximal key.  Returns
-- a potentially empty map ('IntMap'), because we might end up deleting the
-- final key in the map if the function returns 'Nothing'.  See 'adjustMax'
-- for a version that can guarantee that we return a non-empty map.
--
-- > updateMax (\ a -> Just ("X" ++ a)) (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.fromList [(3, "b"), (5, "Xa")]
-- > updateMax (\ _ -> Nothing)         (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.singleton 3 "b"
updateMax :: (a -> Maybe a) -> NEIntMap a -> IntMap a
updateMax f = updateMaxWithKey (const f)
{-# INLINE updateMax #-}

-- | /O(log n)/. A version of 'updateMax' that disallows deletion, allowing
-- us to guarantee that the result is also non-empty.
adjustMax :: (a -> a) -> NEIntMap a -> NEIntMap a
adjustMax f = adjustMaxWithKey (const f)
{-# INLINE adjustMax #-}

-- | /O(log n)/. Update the value at the maximal key.  Returns
-- a potentially empty map ('IntMap'), because we might end up deleting the
-- final key in the map if the function returns 'Nothing'. See
-- 'adjustMaxWithKey' for a version that guaruntees a non-empty map.
--
-- > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.fromList [(3,"3:b"), (5,"a")]
-- > updateMinWithKey (\ _ _ -> Nothing)                     (fromList ((5,"a") :| [(3,"b")])) == Data.IntMap.singleton 5 "a"
updateMaxWithKey :: (Key -> a -> Maybe a) -> NEIntMap a -> IntMap a
updateMaxWithKey f (NEIntMap k v m)
    | M.null m  = maybe m (M.singleton k) $ f k v
    | otherwise = insertMinMap k v
                . M.updateMaxWithKey f
                $ m
{-# INLINE updateMaxWithKey #-}

-- | /O(log n)/. A version of 'updateMaxWithKey' that disallows deletion,
-- allowing us to guarantee that the result is also non-empty.
adjustMaxWithKey :: (Key -> a -> a) -> NEIntMap a -> NEIntMap a
adjustMaxWithKey f (NEIntMap k0 v m)
    | M.null m  = NEIntMap k0 (f k0 v) m
    | otherwise = insertMapMin k0 v
                . M.updateMaxWithKey (\k -> Just . f k)
                $ m
{-# INLINE adjustMaxWithKey #-}

-- | /O(1)/. Retrieves the value associated with minimal key of the
-- map, and the map stripped of that element.  It is constant-time, so has
-- better asymptotics than @Data.IntMap.minView@ for 'IntMap'.
--
-- Note that unlike @Data.IntMap.minView@ for 'IntMap', this cannot ever fail,
-- so doesn't need to return in a 'Maybe'.  However, the result 'IntMap' is
-- potentially empty, since the original map might have contained just
-- a single item.
--
-- > minView (fromList ((5,"a") :| [(3,"b")])) == ("b", Data.IntMap.singleton 5 "a")
minView :: NEIntMap a -> (a, IntMap a)
minView = first snd . deleteFindMin
{-# INLINE minView #-}

-- | /O(1)/. Delete and find the minimal key-value pair.  It is
-- constant-time, so has better asymptotics that @Data.IntMap.minView@ for
-- 'IntMap'.
--
-- Note that unlike @Data.IntMap.deleteFindMin@ for 'IntMap', this cannot ever
-- fail, and so is a total function. However, the result 'IntMap' is
-- potentially empty, since the original map might have contained just
-- a single item.
--
-- > deleteFindMin (fromList ((5,"a") :| [(3,"b"), (10,"c")])) == ((3,"b"), Data.IntMap.fromList [(5,"a"), (10,"c")])
deleteFindMin :: NEIntMap a -> ((Key, a), IntMap a)
deleteFindMin (NEIntMap k v m) = ((k, v), m)
{-# INLINE deleteFindMin #-}

-- | /O(log n)/. Retrieves the value associated with maximal key of the
-- map, and the map stripped of that element.
--
-- Note that unlike @Data.IntMap.maxView@ from 'IntMap', this cannot ever fail,
-- so doesn't need to return in a 'Maybe'.  However, the result 'IntMap' is
-- potentially empty, since the original map might have contained just
-- a single item.
--
-- > maxView (fromList ((5,"a") :| [(3,"b")])) == ("a", Data.IntMap.singleton 3 "b")
maxView :: NEIntMap a -> (a, IntMap a)
maxView = first snd . deleteFindMax
{-# INLINE maxView #-}

-- | /O(log n)/. Delete and find the minimal key-value pair.
--
-- Note that unlike @Data.IntMap.deleteFindMax@ for 'IntMap', this cannot ever
-- fail, and so is a total function. However, the result 'IntMap' is
-- potentially empty, since the original map might have contained just
-- a single item.
--
-- > deleteFindMax (fromList ((5,"a") :| [(3,"b"), (10,"c")])) == ((10,"c"), Data.IntMap.fromList [(3,"b"), (5,"a")])
deleteFindMax :: NEIntMap a -> ((Key, a), IntMap a)
deleteFindMax (NEIntMap k v m) = maybe ((k, v), M.empty) (second (insertMinMap k v))
                            . M.maxViewWithKey
                            $ m
{-# INLINE deleteFindMax #-}

-- ---------------------------
-- Combining functions
-- ---------------------------
--
-- Code comes from "Data.Map.Internal" from containers, modified slightly
-- to work with NonEmpty
--
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008

combineEq :: NonEmpty (Key, b) -> NonEmpty (Key, b)
combineEq = \case
    x :| []       -> x :| []
    x :| xx@(_:_) -> go x xx
  where
    go z [] = z :| []
    go z@(kz,_) (x@(kx,xx):xs')
      | kx==kz    = go (kx,xx) xs'
      | otherwise = z NE.<| go x xs'

combineEqWith
    :: (Key -> b -> b -> b)
    -> NonEmpty (Key, b)
    -> NonEmpty (Key, b)
combineEqWith f = \case
    x :| []       -> x :| []
    x :| xx@(_:_) -> go x xx
  where
    go z [] = z :| []
    go z@(kz,zz) (x@(kx,xx):xs')
      | kx==kz    = let yy = f kx xx zz in go (kx,yy) xs'
      | otherwise = z NE.<| go x xs'
