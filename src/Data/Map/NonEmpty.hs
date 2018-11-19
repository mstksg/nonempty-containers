{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.Map.NonEmpty (
  -- * Map type
    NEMap
  , nonEmptyMap
  , toMap
  , insertMap
  , insertMapWith
  , insertMapWithKey
  , insertMapMin
  , insertMapMax

  -- * Construction
  , empty
  , singleton
  -- , fromSet

  -- ** From Unordered Lists
  , fromList
  , fromListWith
  , fromListWithKey

  -- ** From Ascending Lists
  , fromAscList
  , fromAscListWith
  , fromAscListWithKey
  , fromDistinctAscList

  -- -- ** From Descending Lists
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

  -- ** Size
  , null
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

  -- -- ** Intersection
  , intersection
  , intersectionWith
  , intersectionWithKey

  -- -- ** Unsafe general combining function
  -- , mergeWithKey

  -- * Traversal
  -- ** Map
  , map
  , mapWithKey
  , traverseWithKey
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
  , foldrWithKey
  , foldlWithKey
  , foldMapWithKey

  -- ** Strict folds
  , foldr'
  , foldr1'
  , foldl'
  , foldl1
  , foldl1'
  , foldrWithKey'
  , foldlWithKey'

  -- * Conversion
  , elems
  , keys
  , assocs
  -- , keysSet

  -- ** Lists
  , toNonEmpty

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

  -- * Submap
  , isSubmapOf, isSubmapOfBy
  , isProperSubmapOf, isProperSubmapOfBy

  -- -- * Indexed
  , lookupIndex
  , findIndex
  , elemAt
  , adjustAt
  , updateAt
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
  , updateMinWithKey
  , updateMaxWithKey
  , minView
  , maxView

  -- * Debugging
  , valid
  ) where

import           Control.Applicative
import           Data.Bifunctor
import           Data.Function
import           Data.Functor.Apply
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Map                   (Map)
import           Data.Map.NonEmpty.Internal
import           Data.Maybe hiding          (mapMaybe)
import           Data.Semigroup
import           Data.Semigroup.Foldable    (Foldable1)
import           Prelude hiding             (lookup, foldr1, foldl1, foldr, foldl, filter, map, take, drop, splitAt)
import qualified Data.Foldable              as F
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Semigroup.Foldable    as F1
import qualified Data.Set                   as S

singleton :: k -> a -> NEMap k a
singleton k v = NEMap k v M.empty

-- fromSet
--     :: (k -> a)
--     -> NESet k
--     -> NEMap k a
-- fromSet f (NESet k ks) = NEMap k (f k) (M.fromSet f ks)

lookup
    :: Ord k
    => k
    -> NEMap k a
    -> Maybe a
lookup k (NEMap k0 v m) = case compare k k0 of
    LT -> Nothing
    EQ -> Just v
    GT -> M.lookup k m

(!?) :: Ord k => NEMap k a -> k -> Maybe a
(!?) = flip lookup

(!) :: Ord k => NEMap k a -> k -> a
(!) m k = fromMaybe e $ m !? k
  where
    e = error "NEMap.!: given key is not an element in the map"

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

member :: Ord k => k -> NEMap k a -> Bool
member k (NEMap k0 _ m) = case compare k k0 of
    LT -> False
    EQ -> True
    GT -> M.member k m

notMember :: Ord k => k -> NEMap k a -> Bool
notMember k = not . member k

lookupLT :: Ord k => k -> NEMap k a -> Maybe (k, a)
lookupLT k (NEMap k0 v m) = case compare k k0 of
    LT -> Nothing
    EQ -> Nothing
    GT -> M.lookupLT k m <|> Just (k0, v)

lookupGT :: Ord k => k -> NEMap k a -> Maybe (k, a)
lookupGT k (NEMap k0 v m) = case compare k k0 of
    LT -> Just (k0, v)
    EQ -> M.lookupMin m
    GT -> M.lookupGT k m

lookupLE :: Ord k => k -> NEMap k a -> Maybe (k, a)
lookupLE k (NEMap k0 v m) = case compare k k0 of
    LT -> Nothing
    EQ -> Just (k, v)
    GT -> M.lookupLT k m <|> Just (k0, v)

lookupGE :: Ord k => k -> NEMap k a -> Maybe (k, a)
lookupGE k (NEMap k0 v m) = case compare k k0 of
    LT -> Just (k0, v)
    EQ -> Just (k , v)
    GT -> M.lookupGT k m

unionWith
    :: Ord k
    => (a -> a -> a)
    -> NEMap k a
    -> NEMap k a
    -> NEMap k a
unionWith f n1@(NEMap k1 v1 m1) n2@(NEMap k2 v2 m2) = case compare k1 k2 of
    LT -> NEMap k1 v1        . M.unionWith f m1 . toMap $ n2
    EQ -> NEMap k1 (f v1 v2) . M.unionWith f m1 . toMap $ n2
    GT -> NEMap k2 v2        . M.unionWith f (toMap n1) $ m2

unionWithKey
    :: Ord k
    => (k -> a -> a -> a)
    -> NEMap k a
    -> NEMap k a
    -> NEMap k a
unionWithKey f n1@(NEMap k1 v1 m1) n2@(NEMap k2 v2 m2) = case compare k1 k2 of
    LT -> NEMap k1 v1           . M.unionWithKey f m1 . toMap $ n2
    EQ -> NEMap k1 (f k1 v1 v2) . M.unionWithKey f m1 . toMap $ n2
    GT -> NEMap k2 v2           . M.unionWithKey f (toMap n1) $ m2

unions
    :: (Foldable1 f, Ord k)
    => f (NEMap k a)
    -> NEMap k a
unions (F1.toNonEmpty->(m :| ms)) = F.foldl' union m ms

unionsWith
    :: (Foldable1 f, Ord k)
    => (a -> a -> a)
    -> f (NEMap k a)
    -> NEMap k a
unionsWith f (F1.toNonEmpty->(m :| ms)) = F.foldl' (unionWith f) m ms

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

(\\)
    :: Ord k
    => NEMap k a
    -> NEMap k b
    -> Map k a
(\\) = difference

differenceWith
    :: Ord k
    => (a -> b -> Maybe a)
    -> NEMap k a
    -> NEMap k b
    -> Map k a
differenceWith f = differenceWithKey (const f)

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

intersectionWith
    :: Ord k
    => (a -> b -> c)
    -> NEMap k a
    -> NEMap k b
    -> Map k c
intersectionWith f = intersectionWithKey (const f)

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



foldrWithKey :: (k -> a -> b -> b) -> b -> NEMap k a -> b
foldrWithKey f z (NEMap k v m) = f k v . M.foldrWithKey f z $ m

foldrWithKey' :: (k -> a -> b -> b) -> b -> NEMap k a -> b
foldrWithKey' f z (NEMap k v m) = f k v y
  where
    !y = M.foldrWithKey f z m

foldlWithKey :: (a -> k -> b -> a) -> a -> NEMap k b -> a
foldlWithKey f z (NEMap k v m) = M.foldlWithKey f (f z k v) m

foldlWithKey' :: (a -> k -> b -> a) -> a -> NEMap k b -> a
foldlWithKey' f z (NEMap k v m) = M.foldlWithKey' f x m
  where
    !x = f z k v

keys :: NEMap k a -> NonEmpty k
keys (NEMap k _ m) = k :| M.keys m

assocs :: NEMap k a -> NonEmpty (k, a)
assocs = toNonEmpty

-- keysSet :: NEMap k a -> NESet k
-- keysSet (NEMap k _ m) = NESet k (M.keysSet m)

mapWithKey :: (k -> a -> b) -> NEMap k a -> NEMap k b
mapWithKey f (NEMap k v m) = NEMap k (f k v) (M.mapWithKey f m)

toNonEmpty :: NEMap k a -> NonEmpty (k, a)
toNonEmpty (NEMap k v m) = (k,v) :| M.toList m

toAscList :: NEMap k a -> NonEmpty (k, a)
toAscList = toNonEmpty

toDescList :: NEMap k a -> NonEmpty (k, a)
toDescList (NEMap k v m) = maybe kv0 (<> kv0)
                         . NE.nonEmpty
                         . M.toDescList
                         $ m
  where
    kv0 = (k, v) :| []

nonEmptyMap :: Map k a -> Maybe (NEMap k a)
nonEmptyMap m = uncurry (\(k, v) -> NEMap k v) <$> M.minViewWithKey m

insertMap :: Ord k => k -> a -> Map k a -> NEMap k a
insertMap k v = maybe (singleton k v) (insert k v) . nonEmptyMap

insertMapWith
    :: Ord k
    => (a -> a -> a)
    -> k
    -> a
    -> Map k a
    -> NEMap k a
insertMapWith f k v = maybe (singleton k v) (insertWith f k v)
                    . nonEmptyMap

insertMapWithKey
    :: Ord k
    => (k -> a -> a -> a)
    -> k
    -> a
    -> Map k a
    -> NEMap k a
insertMapWithKey f k v = maybe (singleton k v) (insertWithKey f k v)
                       . nonEmptyMap

insertMapMin
    :: k
    -> a
    -> Map k a
    -> NEMap k a
insertMapMin = NEMap

insertMapMax
    :: k
    -> a
    -> Map k a
    -> NEMap k a
insertMapMax k v = maybe (singleton k v) go
                 . nonEmptyMap
  where
    go (NEMap k0 v0 m0) = NEMap k0 v0 . insertMaxMap k v $ m0


-- this could be implemented using insertWith, but containers implements
-- a custom fromList, so we can use this instead to take advantage of this
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

-- this could be implemented using insertWithKey, but containers implements
-- a custom fromList, so we can use this instead to take advantage of this
insertWith
    :: Ord k
    => (a -> a -> a)
    -> k
    -> a
    -> NEMap k a
    -> NEMap k a
insertWith f k v n@(NEMap k0 v0 m) = case compare k k0 of
    LT -> NEMap k  v        . toMap            $ n
    EQ -> NEMap k  (f v v0) m
    GT -> NEMap k0 v0       $ M.insertWith f k v m

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

fromList :: Ord k => NonEmpty (k, a) -> NEMap k a
fromList ((k, v) :| xs) = maybe (singleton k v) (insertWith (const id) k v)
                        . nonEmptyMap
                        $ M.fromList xs

fromListWith
    :: Ord k
    => (a -> a -> a)
    -> NonEmpty (k, a)
    -> NEMap k a
fromListWith f = fromListWithKey (const f)

fromListWithKey
    :: Ord k
    => (k -> a -> a -> a)
    -> NonEmpty (k, a)
    -> NEMap k a
fromListWithKey f ((k0, v0) :| xs) = F.foldl' go (singleton k0 v0) xs
  where
    go m (k, v) = insertWithKey f k v m

fromAscList
    :: Eq k
    => NonEmpty (k, a)
    -> NEMap k a
fromAscList = fromDistinctAscList . combineEq

fromAscListWith
    :: Eq k
    => (a -> a -> a)
    -> NonEmpty (k, a)
    -> NEMap k a
fromAscListWith f = fromAscListWithKey (const f)

fromAscListWithKey
    :: Eq k
    => (k -> a -> a -> a)
    -> NonEmpty (k, a)
    -> NEMap k a
fromAscListWithKey f = fromDistinctAscList . combineEqWith f

fromDistinctAscList :: NonEmpty (k, a) -> NEMap k a
fromDistinctAscList ((k, v) :| xs) = insertMapMin k v
                                   . M.fromDistinctAscList
                                   $ xs

fromDescList
    :: Eq k
    => NonEmpty (k, a)
    -> NEMap k a
fromDescList = fromDistinctDescList . combineEq

fromDescListWith
    :: Eq k
    => (a -> a -> a)
    -> NonEmpty (k, a)
    -> NEMap k a
fromDescListWith f = fromDescListWithKey (const f)

fromDescListWithKey
    :: Eq k
    => (k -> a -> a -> a)
    -> NonEmpty (k, a)
    -> NEMap k a
fromDescListWithKey f = fromDistinctDescList . combineEqWith f

fromDistinctDescList :: NonEmpty (k, a) -> NEMap k a
fromDistinctDescList ((k, v) :| xs) = insertMapMax k v
                                    . M.fromDistinctDescList
                                    $ xs

delete :: Ord k => k -> NEMap k a -> Map k a
delete k n@(NEMap k0 _ m)
    | k == k0   = m
    | otherwise = toMap n

adjust
    :: Ord k
    => (a -> a)
    -> k
    -> NEMap k a
    -> NEMap k a
adjust f = adjustWithKey (const f)

adjustWithKey
    :: Ord k
    => (k -> a -> a)
    -> k
    -> NEMap k a
    -> NEMap k a
adjustWithKey f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> n
    EQ -> NEMap k0 (f k v) m
    GT -> NEMap k0 v . M.adjustWithKey f k $ m

update
    :: Ord k
    => (a -> Maybe a)
    -> k
    -> NEMap k a
    -> Map k a
update f = updateWithKey (const f)

updateWithKey
    :: Ord k
    => (k -> a -> Maybe a)
    -> k
    -> NEMap k a
    -> Map k a
updateWithKey f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> toMap n
    EQ -> maybe m (flip (insertMinMap k) m) . f k $ v
    GT -> insertMinMap k0 v . M.updateWithKey f k $ m

updateLookupWithKey
    :: Ord k
    => (k -> a -> Maybe a)
    -> k
    -> NEMap k a
    -> (Maybe a, Map k a)
updateLookupWithKey f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> (Nothing, toMap n)
    EQ -> (Just v , maybe m (flip (insertMinMap k) m) . f k $ v)
    GT -> fmap (insertMinMap k0 v) . M.updateLookupWithKey f k $ m

alter
    :: Ord k
    => (Maybe a -> Maybe a)
    -> k
    -> NEMap k a
    -> Map k a
alter f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> ($ toMap n) . maybe id (insertMinMap k) $ f Nothing
    EQ -> ($ m      ) . maybe id (insertMinMap k) $ f (Just v)
    GT -> insertMinMap k0 v . M.alter f k $ m

-- TODO: is this faster than just toMapping?
alterF
    :: (Ord k, Functor f)
    => (Maybe a -> f (Maybe a))
    -> k
    -> NEMap k a
    -> f (Map k a)
alterF f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> ($ toMap n) . maybe id (insertMinMap k) <$> f Nothing
    EQ -> ($ m      ) . maybe id (insertMinMap k) <$> f (Just v)
    GT -> insertMinMap k0 v <$> M.alterF f k m

-- | Variant of 'alter' that disallows deletion.  Allows us to guarantee
-- that the result is also a non-empty Map.
alter'
    :: Ord k
    => (Maybe a -> a)
    -> k
    -> NEMap k a
    -> NEMap k a
alter' f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> NEMap k (f Nothing) . toMap      $ n
    EQ -> NEMap k (f (Just v))             $ m
    GT -> NEMap k v . M.alter (Just . f) k $ m

-- | Variant of 'alterF' that disallows deletion.  Allows us to guarantee
-- that the result is also a non-empty Map.
alterF'
    :: (Ord k, Functor f)
    => (Maybe a -> f a)
    -> k
    -> NEMap k a
    -> f (NEMap k a)
alterF' f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> flip (NEMap k) (toMap n) <$> f Nothing
    EQ -> flip (NEMap k) m         <$> f (Just v)
    GT -> NEMap k0 v <$> M.alterF (fmap Just . f) k m

map :: (a -> b) -> NEMap k a -> NEMap k b
map = fmap

-- TODO: benchmark against M.maxView version
traverseMaybeWithKey
    :: Apply t
    => (k -> a -> t (Maybe b))
    -> NEMap k a
    -> t (Map k b)
traverseMaybeWithKey f (NEMap k0 v m0) = case runMaybeApply m1 of
    Left  m2 -> combine <$> f k0 v <.> m2
    Right m2 -> (`combine` m2) <$> f k0 v
  where
    m1 = M.traverseMaybeWithKey (\k -> MaybeApply . Left . f k) m0
    combine Nothing   = id
    combine (Just v') = insertMinMap k0 v'

mapAccum
    :: (a -> b -> (a, c))
    -> a
    -> NEMap k b
    -> (a, NEMap k c)
mapAccum f = mapAccumWithKey (\x _ -> f x)

mapAccumWithKey
    :: (a -> k -> b -> (a, c))
    -> a
    -> NEMap k b
    -> (a, NEMap k c)
mapAccumWithKey f z0 (NEMap k v m) = (z2, NEMap k v' m')
  where
    (z1, v') = f z0 k v
    (z2, m') = M.mapAccumWithKey f z1 m

mapAccumRWithKey
    :: (a -> k -> b -> (a, c))
    -> a
    -> NEMap k b
    -> (a, NEMap k c)
mapAccumRWithKey f z0 (NEMap k v m) = (z2, NEMap k v' m')
  where
    (z1, m') = M.mapAccumWithKey f z0 m
    (z2, v') = f z1 k v

-- Result can be smaller, but not empty
mapKeys
    :: Ord k2
    => (k1 -> k2)
    -> NEMap k1 a
    -> NEMap k2 a
mapKeys f (NEMap k v m) = maybe (singleton (f k) v) (insertWith (const id) (f k) v)     -- BAD ORDER?
                        . nonEmptyMap
                        $ M.mapKeys f m

-- Result can be smaller, but not empty
mapKeysWith
    :: Ord k2
    => (a -> a -> a)
    -> (k1 -> k2)
    -> NEMap k1 a
    -> NEMap k2 a
mapKeysWith c f (NEMap k v m) = maybe (singleton (f k) v) (insertWith (flip c) (f k) v)     -- THIS BAD! BAD ORDER!
                              . nonEmptyMap
                              . M.mapKeysWith c f
                              $ m

-- Result can be smaller, but not empty
mapKeysMonotonic
    :: (k1 -> k2)
    -> NEMap k1 a
    -> NEMap k2 a
mapKeysMonotonic f (NEMap k v m) = NEMap (f k) v
                                 . M.mapKeysMonotonic f
                                 $ m

filter
    :: (a -> Bool)
    -> NEMap k a
    -> Map k a
filter f (NEMap k v m)
    | f v       = insertMinMap k v . M.filter f $ m
    | otherwise = M.filter f m

filterWithKey
    :: (k -> a -> Bool)
    -> NEMap k a
    -> Map k a
filterWithKey f (NEMap k v m)
    | f k v     = insertMinMap k v . M.filterWithKey f $ m
    | otherwise = M.filterWithKey f m

restrictKeys
    :: Ord k
    => NEMap k a
    -> S.Set k
    -> Map k a
restrictKeys (NEMap k v m) ks
    | S.member k ks = insertMinMap k v . M.restrictKeys m $ ks
    | otherwise     = M.restrictKeys m ks

withoutKeys
    :: Ord k
    => NEMap k a
    -> S.Set k
    -> Map k a
withoutKeys (NEMap k v m) ks
    | S.member k ks = M.restrictKeys m ks
    | otherwise     = insertMinMap k v . M.restrictKeys m $ ks

-- Requires Ord k
partition
    :: (a -> Bool)
    -> NEMap k a
    -> (Map k a, Map k a)
partition f = partitionWithKey (const f)

-- Requires Ord k
partitionWithKey
    :: (k -> a -> Bool)
    -> NEMap k a
    -> (Map k a, Map k a)
partitionWithKey f (NEMap k v m0)
    | f k v     = (insertMinMap k v m1, m2                 )
    | otherwise = (m1                 , insertMinMap k v m2)
  where
    (m1, m2) = M.partitionWithKey f m0

takeWhileAntitone
    :: (k -> Bool)
    -> NEMap k a
    -> Map k a
takeWhileAntitone f (NEMap k v m)
    | f k       = insertMinMap k v . M.takeWhileAntitone f $ m
    | otherwise = M.empty

dropWhileAntitone
    :: (k -> Bool)
    -> NEMap k a
    -> Map k a
dropWhileAntitone f n@(NEMap k _ m)
    | f k       = M.dropWhileAntitone f m
    | otherwise = toMap n

spanAntitone
    :: (k -> Bool)
    -> NEMap k a
    -> (Map k a, Map k a)
spanAntitone f n@(NEMap k v m)
    | f k       = first (insertMinMap k v) . M.spanAntitone f $ m
    | otherwise = (M.empty, toMap n)

mapMaybe
    :: (a -> Maybe b)
    -> NEMap k a
    -> Map k b
mapMaybe f = mapMaybeWithKey (const f)

mapMaybeWithKey
    :: (k -> a -> Maybe b)
    -> NEMap k a
    -> Map k b
mapMaybeWithKey f (NEMap k v m) = ($ M.mapMaybeWithKey f m)
                                . maybe id (insertMinMap k)
                                $ f k v

mapEither
    :: (a -> Either b c)
    -> NEMap k a
    -> (Map k b, Map k c)
mapEither f = mapEitherWithKey (const f)

mapEitherWithKey
    :: (k -> a -> Either b c)
    -> NEMap k a
    -> (Map k b, Map k c)
mapEitherWithKey f (NEMap k v m0) = case f k v of
    Left  v' -> (insertMinMap k v' m1, m2                  )
    Right v' -> (m1                  , insertMinMap k v' m2)
  where
    (m1, m2) = M.mapEitherWithKey f m0

split :: Ord k => k -> NEMap k a -> (Map k a, Map k a)
split k n@(NEMap k0 v m) = case compare k k0 of
    LT -> (M.empty, toMap n)
    EQ -> (M.empty, m      )
    GT -> first (insertMinMap k0 v) . M.split k $ m

splitLookup
    :: Ord k
    => k
    -> NEMap k a
    -> (Map k a, Maybe a, Map k a)
splitLookup k n@(NEMap k0 v m0) = case compare k k0 of
    LT -> (M.empty, Nothing, toMap n)
    EQ -> (M.empty, Just v , m0     )
    GT -> let (m1, x, m2) = M.splitLookup k m0
          in  (insertMinMap k0 v m1, x, m2)

isSubmapOf :: (Ord k, Eq a) => NEMap k a -> NEMap k a -> Bool
isSubmapOf = isSubmapOfBy (==)

-- performance benefit: can short-circuit, skip an insert
isSubmapOfBy
    :: Ord k
    => (a -> b -> Bool)
    -> NEMap k a
    -> NEMap k b
    -> Bool
isSubmapOfBy f (toMap->m0) (NEMap k v m1) = kvSub
                                         && M.isSubmapOfBy f m0 m1
  where
    kvSub = case M.lookup k m0 of
      Just v0 -> f v0 v
      Nothing -> False

-- is there a better way to do this?
isProperSubmapOf :: (Ord k, Eq a) => NEMap k a -> NEMap k a -> Bool
isProperSubmapOf = isProperSubmapOfBy (==)

-- is there a better way to do this?
isProperSubmapOfBy
    :: Ord k
    => (a -> b -> Bool)
    -> NEMap k a
    -> NEMap k b
    -> Bool
isProperSubmapOfBy f m1 m2 = M.isProperSubmapOfBy f (toMap m1) (toMap m2)

lookupIndex
    :: Ord k
    => k
    -> NEMap k a
    -> Maybe Int
lookupIndex k (NEMap k0 _ m) = case compare k k0 of
    LT -> Nothing
    EQ -> Just 0
    GT -> (+ 1) <$> M.lookupIndex k m

findIndex
    :: Ord k
    => k
    -> NEMap k a
    -> Int
findIndex k = fromMaybe e . lookupIndex k
  where
    e = error "NEMap.findIndex: element is not in the map"

elemAt
    :: Int
    -> NEMap k a
    -> (k, a)
elemAt 0 (NEMap k v _) = (k, v)
elemAt n (NEMap _ _ m) = M.elemAt (n - 1) m

-- | Variant of 'updateAt' that disallows deletion.  Allows us to guarantee
-- that the result is also a non-empty Map.
adjustAt
    :: (k -> a -> a)
    -> Int
    -> NEMap k a
    -> NEMap k a
adjustAt f 0 (NEMap k0 v m) = NEMap k0 (f k0 v) m
adjustAt f n (NEMap k0 v m) = NEMap k0 v
                            . M.updateAt (\k -> Just . f k) (n - 1)
                            $ m

updateAt
    :: (k -> a -> Maybe a)
    -> Int
    -> NEMap k a
    -> Map k a
updateAt f 0 (NEMap k v m) = maybe m (flip (insertMinMap k) m) $ f k v
updateAt f n (NEMap k v m) = insertMinMap k v . M.updateAt f (n - 1) $ m

deleteAt
    :: Int
    -> NEMap k a
    -> Map k a
deleteAt 0 (NEMap _ _ m) = m
deleteAt n (NEMap k v m) = insertMinMap k v . M.deleteAt (n - 1) $ m

take
    :: Int
    -> NEMap k a
    -> Map k a
take 0 (NEMap _ _ _) = M.empty
take n (NEMap k v m) = insertMinMap k v . M.take (n - 1) $ m

drop
    :: Int
    -> NEMap k a
    -> Map k a
drop 0 n             = toMap n
drop n (NEMap _ _ m) = M.drop (n - 1) m

splitAt
    :: Int
    -> NEMap k a
    -> (Map k a, Map k a)
splitAt 0 n             = (M.empty, toMap n)
splitAt n (NEMap k v m) = first (insertMinMap k v) . M.splitAt (n - 1) $ m

findMin :: NEMap k a -> (k, a)
findMin (NEMap k v _) = (k, v)

findMax :: NEMap k a -> (k, a)
findMax (NEMap k v m) = fromMaybe (k, v) . M.lookupMax $ m

deleteMin :: NEMap k a -> Map k a
deleteMin (NEMap _ _ m) = m

deleteMax :: NEMap k a -> Map k a
deleteMax (NEMap k v m) = insertMinMap k v . M.deleteMax $ m

updateMin :: (a -> Maybe a) -> NEMap k a -> Map k a
updateMin f = updateMinWithKey (const f)

updateMinWithKey :: (k -> a -> Maybe a) -> NEMap k a -> Map k a
updateMinWithKey f (NEMap k v m) = ($ m) . maybe id (insertMinMap k) $ f k v

updateMax :: (a -> Maybe a) -> NEMap k a -> Map k a
updateMax f = updateMaxWithKey (const f)

updateMaxWithKey :: (k -> a -> Maybe a) -> NEMap k a -> Map k a
updateMaxWithKey f (NEMap k v m) = insertMinMap k v
                                 . M.updateMaxWithKey f
                                 $ m

-- deleteFindMin
minView :: NEMap k a -> (a, Map k a)
minView = first snd . deleteFindMin

deleteFindMin :: NEMap k a -> ((k, a), Map k a)
deleteFindMin (NEMap k v m) = ((k, v), m)

-- deleteFindMax
maxView :: NEMap k a -> (a, Map k a)
maxView = first snd . deleteFindMax

-- requires Ord
deleteFindMax :: NEMap k a -> ((k, a), Map k a)
deleteFindMax (NEMap k v m) = maybe ((k, v), M.empty) (second (insertMinMap k v))
                            . M.maxViewWithKey
                            $ m

valid :: Ord k => NEMap k a -> Bool
valid (NEMap k _ m) = all (k <) (M.keys m) && M.valid m

-- Combining functions

combineEq :: Eq a => NonEmpty (a, b) -> NonEmpty (a, b)
combineEq (x :| xs) = case NE.nonEmpty xs of
    Nothing -> x :| []
    Just ys -> go x ys
  where
    go z@(kz,_) (y@(ky,_) :| ys)
      | ky == kz  = case NE.nonEmpty ys of
          Nothing -> y :| []
          Just zs -> go y zs
      | otherwise = case NE.nonEmpty ys of
          Nothing -> z :| [y]
          Just zs -> z :| F.toList (go y zs)

combineEqWith :: Eq a => (a -> b -> b -> b) -> NonEmpty (a, b) -> NonEmpty (a, b)
combineEqWith f (x :| xs) = case NE.nonEmpty xs of
    Nothing -> x :| []
    Just ys -> go x ys
  where
    go z@(kz,zz) (y@(ky,yy) :| ys)
      | ky == kz  = case NE.nonEmpty ys of
          Nothing -> y :| []
          Just zs -> let yy' = f ky yy zz
                     in  go (ky, yy') zs
      | otherwise = case NE.nonEmpty ys of
          Nothing -> z :| [y]
          Just zs -> z :| F.toList (go y zs)
