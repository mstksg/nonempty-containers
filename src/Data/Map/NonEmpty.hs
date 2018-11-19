{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.Map.NonEmpty (
  -- * Map type
    NEMap(..)
  , nonEmptyMap
  , toMap
  , insertMap
  , insertMapWith
  , insertMapWithKey
  , insertMapMin

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
  -- , fromDescList
  -- , fromDescListWith
  -- , fromDescListWithKey
  -- , fromDistinctDescList

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

  -- -- ** Difference
  -- , difference
  -- , (\\)
  -- , differenceWith
  -- , differenceWithKey

  -- -- ** Intersection
  -- , intersection
  -- , intersectionWith
  -- , intersectionWithKey

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
  -- , mapAccumRWithKey
  , mapKeys
  , mapKeysWith
  , mapKeysMonotonic

  -- * Folds
  -- , foldr
  , foldl
  -- , foldrWithKey
  -- , foldlWithKey
  , foldMapWithKey

  -- ** Strict folds
  -- , foldr'
  , foldr1'
  -- , foldl'
  , foldl1'
  -- , foldrWithKey'
  -- , foldlWithKey'

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
  -- , lookupIndex
  -- , findIndex
  -- , elemAt
  -- , updateAt
  -- , deleteAt
  -- , take
  -- , drop
  -- , splitAt

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
  , valid
  ) where

import           Control.Applicative
import           Data.Bifunctor
import           Data.Function
import           Data.Functor.Apply
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Maybe hiding          (mapMaybe)
import           Data.Semigroup.Foldable    (Foldable1(fold1))
import           Data.Semigroup.Traversable (Traversable1(..))
import           Prelude hiding             (lookup, foldr1, foldr, filter, map)
import qualified Data.Foldable              as F
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Semigroup.Foldable    as F1
import qualified Data.Set                   as S

data NEMap k a =
    NEMap !k   -- ^ invariant: must be smaller than smallest key in map
          a
          !(M.Map k a)
  deriving (Eq, Ord, Functor)

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

size :: NEMap k a -> Int
size (NEMap _ _ m) = 1 + M.size m

union
    :: Ord k
    => NEMap k a
    -> NEMap k a
    -> NEMap k a
union n1@(NEMap k1 v1 m1) n2@(NEMap k2 v2 m2) = case compare k1 k2 of
    LT -> NEMap k1 v1 . M.union m1 . toMap $ n2
    EQ -> NEMap k1 v1 . M.union m1 . toMap $ n2
    GT -> NEMap k2 v2 . M.union (toMap n1) $ m2

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

foldr1 :: (a -> a -> a) -> NEMap k a -> a
foldr1 f (NEMap _ v m) = case M.maxView m of
    Nothing      -> v
    Just (y, m') -> f v (M.foldr f y m')

foldMap1 :: Semigroup m => (a -> m) -> NEMap k a -> m
foldMap1 f (NEMap _ v m) = case M.maxView m of
    Nothing     -> f v
    Just (x,m') -> f v <> M.foldr (\c d -> f c <> d) (f x) m'

foldMapWithKey
    :: Semigroup m
    => (k -> a -> m)
    -> NEMap k a
    -> m
foldMapWithKey f (NEMap k v m) = case M.maxView m of
    Nothing      -> f k v
    Just (x, m') -> f k v <> M.foldrWithKey (\k' c d -> f k' c <> d) (f k x) m'

-- | Left-biased union
instance Ord k => Semigroup (NEMap k a) where
    (<>) = union

-- | Traverses elements in order ascending keys
instance Foldable (NEMap k) where
    fold (NEMap _ v m) = v <> F.fold m
    foldMap f (NEMap _ v m) = f v <> foldMap f m
    foldr f a (NEMap _ v m) = case M.maxView m of
        Nothing      -> f v a
        Just (y, m') -> f v (M.foldr f (f y a) m')
    foldl f a (NEMap _ v m) = M.foldl f (f a v) m
    foldr1 = foldr1
    foldl1 f (NEMap _ v m) = M.foldl f v m
    null _ = False
    length = size
    elem   = elem
    minimum = foldl1' min
    maximum = foldl1' max

-- | Traverses elements in order ascending keys
instance Traversable (NEMap k) where
    traverse f (NEMap k v m) = NEMap k <$> f v <*> traverse f m
    sequenceA (NEMap k v m)  = NEMap k <$> v <*> sequenceA m

-- | Traverses elements in order ascending keys
instance Foldable1 (NEMap k) where
    toNonEmpty = elems
    fold1 = foldr1 (<>)
    foldMap1 = foldMap1

-- | Traverses elements in order ascending keys
--
-- TODO: use manual fold
instance Traversable1 (NEMap k) where
    traverse1 f = fmap fromDistinctAscList . (traverse1 . traverse1) f . toNonEmpty
    sequence1 = fmap fromDistinctAscList . traverse1 sequence1 . toNonEmpty

elems :: NEMap k a -> NonEmpty a
elems (NEMap _ v m) = v :| M.elems m

keys :: NEMap k a -> NonEmpty k
keys (NEMap k _ m) = k :| M.keys m

assocs :: NEMap k a -> NonEmpty (k, a)
assocs = toNonEmpty

-- keysSet :: NEMap k a -> NESet k
-- keysSet (NEMap k _ m) = NESet k (M.keysSet m)

foldl1' :: (a -> a -> a) -> NEMap k a -> a
foldl1' f (NEMap _ v m) = M.foldl' f v m

foldr1' :: (a -> a -> a) -> NEMap k a -> a
foldr1' f (NEMap _ v m) = case M.maxView m of
  Nothing -> v
  Just (y,m') -> let !k = M.foldr' f y m' in f v k

mapWithKey :: (k -> a -> b) -> NEMap k a -> NEMap k b
mapWithKey f (NEMap k v m) = NEMap k (f k v) (M.mapWithKey f m)

-- assocs, toAscList
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

-- maybe we can try to make this not require Ord by doing things manually
toMap :: Ord k => NEMap k a -> M.Map k a
toMap (NEMap k v m) = M.insert k v m

nonEmptyMap :: M.Map k a -> Maybe (NEMap k a)
nonEmptyMap m = uncurry (\(k, v) -> NEMap k v) <$> M.minViewWithKey m

insertMap :: Ord k => k -> a -> M.Map k a -> NEMap k a
insertMap k v = maybe (singleton k v) (insert k v) . nonEmptyMap

insertMapWith
    :: Ord k
    => (a -> a -> a)
    -> k
    -> a
    -> M.Map k a
    -> NEMap k a
insertMapWith f k v = maybe (singleton k v) (insertWith f k v)
                    . nonEmptyMap

insertMapWithKey
    :: Ord k
    => (k -> a -> a -> a)
    -> k
    -> a
    -> M.Map k a
    -> NEMap k a
insertMapWithKey f k v = maybe (singleton k v) (insertWithKey f k v)
                       . nonEmptyMap

insertMapMin
    :: k
    -> a
    -> M.Map k a
    -> NEMap k a
insertMapMin = NEMap

-- this could be implemented using insertWith, but containers implements
-- a custom fromList, so we can use this instead to take advantage of this
insert
    :: Ord k
    => k
    -> a
    -> NEMap k a
    -> NEMap k a
insert k v (NEMap k0 v0 m) = case compare k k0 of
    LT -> NEMap k  v  $ M.insert k0 v0 m
    EQ -> NEMap k  v  m
    GT -> NEMap k0 v0 $ M.insert k  v  m

-- this could be implemented using insertWithKey, but containers implements
-- a custom fromList, so we can use this instead to take advantage of this
insertWith
    :: Ord k
    => (a -> a -> a)
    -> k
    -> a
    -> NEMap k a
    -> NEMap k a
insertWith f k v (NEMap k0 v0 m) = case compare k k0 of
    LT -> NEMap k  v        $ M.insertWith f k0 v0 m
    EQ -> NEMap k  (f v v0) m
    GT -> NEMap k0 v0       $ M.insertWith f k  v  m

insertWithKey
    :: Ord k
    => (k -> a -> a -> a)
    -> k
    -> a
    -> NEMap k a
    -> NEMap k a
insertWithKey f k v (NEMap k0 v0 m) = case compare k k0 of
    LT -> NEMap k  v          $ M.insertWithKey f k0 v0 m
    EQ -> NEMap k  (f k v v0) m
    GT -> NEMap k0 v0         $ M.insertWithKey f k  v  m

insertLookupWithKey
    :: Ord k
    => (k -> a -> a -> a)
    -> k
    -> a
    -> NEMap k a
    -> (Maybe a, NEMap k a)
insertLookupWithKey f k v (NEMap k0 v0 m) = case compare k k0 of
    LT -> (Nothing, NEMap k  v          $ M.insertWithKey f k0 v0 m)
    EQ -> (Just v , NEMap k  (f k v v0) m                          )
    GT -> NEMap k0 v0 <$> M.insertLookupWithKey f k  v m

-- this could be implemented using fromListWith, but containers implements
-- a custom fromList, so we can use this instead to take advantage of this
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
fromListWithKey f ((k, v) :| xs) = maybe (singleton k v) (insertWith (flip (f k)) k v)
                                 . nonEmptyMap
                                 $ M.fromListWithKey f xs

fromAscList
    :: Eq k
    => NonEmpty (k, a)
    -> NEMap k a
fromAscList ((k, v) :| xs) = NEMap k (NE.last (v :| fmap snd ys))
                           . M.fromAscList
                           $ zs
  where
    (ys, zs) = span ((== k) . fst) xs

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
fromAscListWithKey f ((k, v) :| xs) = NEMap k vFinal
                                    . M.fromAscListWithKey f
                                    $ zs
  where
    (ys, zs) = span ((== k) . fst) xs
    vFinal   = F.foldl1 (f k) (v :| fmap snd ys)

fromDistinctAscList :: NonEmpty (k, a) -> NEMap k a
fromDistinctAscList ((k, v) :| xs) = NEMap k v
                                   . M.fromDistinctAscList
                                   $ xs


delete :: Ord k => k -> NEMap k a -> M.Map k a
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
    -> M.Map k a
update f = updateWithKey (const f)

updateWithKey
    :: Ord k
    => (k -> a -> Maybe a)
    -> k
    -> NEMap k a
    -> M.Map k a
updateWithKey f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> toMap n
    EQ -> maybe m (flip (M.insert k) m) . f k $ v
    GT -> M.insert k0 v . M.updateWithKey f k $ m

updateLookupWithKey
    :: Ord k
    => (k -> a -> Maybe a)
    -> k
    -> NEMap k a
    -> (Maybe a, M.Map k a)
updateLookupWithKey f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> (Nothing, toMap n)
    EQ -> (Just v , maybe m (flip (M.insert k) m) . f k $ v)
    GT -> fmap (M.insert k0 v) . M.updateLookupWithKey f k $ m

alter
    :: Ord k
    => (Maybe a -> Maybe a)
    -> k
    -> NEMap k a
    -> M.Map k a
alter f k n@(NEMap k0 v m) = case compare k k0 of
  LT -> ($ toMap n) . maybe id (M.insert k) $ f Nothing
  EQ -> ($ m      ) . maybe id (M.insert k) $ f (Just v)
  GT -> M.insert k0 v . M.alter f k $ m

-- TODO: size-preserving alter
alterF
    :: (Ord k, Functor f)
    => (Maybe a -> f (Maybe a))
    -> k
    -> NEMap k a
    -> f (M.Map k a)
alterF f k n@(NEMap k0 v m) = case compare k k0 of
    LT -> ($ toMap n) . maybe id (M.insert k) <$> f Nothing
    EQ -> ($ m      ) . maybe id (M.insert k) <$> f (Just v)
    GT -> M.insert k0 v <$> M.alterF f k m

map :: (a -> b) -> NEMap k a -> NEMap k b
map = fmap

traverseWithKey
    :: Apply t
    => (k -> a -> t b)
    -> NEMap k a
    -> t (NEMap k b)
traverseWithKey f = fmap fromDistinctAscList
                  . traverse1 (\(k, v) -> (k,) <$> f k v)
                  . toNonEmpty

-- Requires Ord k
traverseMaybeWithKey
    :: (Ord k, Applicative t)
    => (k -> a -> t (Maybe b))
    -> NEMap k a
    -> t (M.Map k b)
traverseMaybeWithKey f (NEMap k v m) =
    combine <$> f k v <*> M.traverseMaybeWithKey f m
  where
    combine Nothing   = id
    combine (Just v') = M.insert k v'

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

-- Requires Ord k
filter
    :: Ord k
    => (a -> Bool)
    -> NEMap k a
    -> M.Map k a
filter f (NEMap k v m)
    | f v       = M.insert k v . M.filter f $ m
    | otherwise = M.filter f m

-- Requires Ord k
filterWithKey
    :: Ord k
    => (k -> a -> Bool)
    -> NEMap k a
    -> M.Map k a
filterWithKey f (NEMap k v m)
    | f k v     = M.insert k v . M.filterWithKey f $ m
    | otherwise = M.filterWithKey f m

restrictKeys
    :: Ord k
    => NEMap k a
    -> S.Set k
    -> M.Map k a
restrictKeys (NEMap k v m) ks
    | S.member k ks = M.insert k v . M.restrictKeys m $ ks
    | otherwise     = M.restrictKeys m ks

withoutKeys
    :: Ord k
    => NEMap k a
    -> S.Set k
    -> M.Map k a
withoutKeys (NEMap k v m) ks
    | S.member k ks = M.restrictKeys m ks
    | otherwise     = M.insert k v . M.restrictKeys m $ ks

-- Requires Ord k
partition
    :: Ord k
    => (a -> Bool)
    -> NEMap k a
    -> (M.Map k a, M.Map k a)
partition f = partitionWithKey (const f)

-- Requires Ord k
partitionWithKey
    :: Ord k
    => (k -> a -> Bool)
    -> NEMap k a
    -> (M.Map k a, M.Map k a)
partitionWithKey f (NEMap k v m0)
    | f k v     = (M.insert k v m1, m2             )
    | otherwise = (m1             , M.insert k v m2)
  where
    (m1, m2) = M.partitionWithKey f m0

takeWhileAntitone
    :: Ord k
    => (k -> Bool)
    -> NEMap k a
    -> M.Map k a
takeWhileAntitone f (NEMap k v m)
    | f k       = M.insert k v . M.takeWhileAntitone f $ m
    | otherwise = M.empty

dropWhileAntitone
    :: Ord k
    => (k -> Bool)
    -> NEMap k a
    -> M.Map k a
dropWhileAntitone f n@(NEMap k _ m)
    | f k       = M.dropWhileAntitone f m
    | otherwise = toMap n

spanAntitone
    :: Ord k
    => (k -> Bool)
    -> NEMap k a
    -> (M.Map k a, M.Map k a)
spanAntitone f n@(NEMap k v m)
    | f k       = first (M.insert k v) . M.spanAntitone f $ m
    | otherwise = (M.empty, toMap n)

mapMaybe
    :: Ord k
    => (a -> Maybe b)
    -> NEMap k a
    -> M.Map k b
mapMaybe f = mapMaybeWithKey (const f)

-- Requires Ord k
mapMaybeWithKey
    :: Ord k
    => (k -> a -> Maybe b)
    -> NEMap k a
    -> M.Map k b
mapMaybeWithKey f (NEMap k v m) = ($ M.mapMaybeWithKey f m)
                                . maybe id (M.insert k)
                                $ f k v

-- Requires Ord k
mapEither
    :: Ord k
    => (a -> Either b c)
    -> NEMap k a
    -> (M.Map k b, M.Map k c)
mapEither f = mapEitherWithKey (const f)

-- Requires Ord k
mapEitherWithKey
    :: Ord k
    => (k -> a -> Either b c)
    -> NEMap k a
    -> (M.Map k b, M.Map k c)
mapEitherWithKey f (NEMap k v m0) = case f k v of
    Left  v' -> (M.insert k v' m1, m2)
    Right v' -> (m1, M.insert k v' m2)
  where
    (m1, m2) = M.mapEitherWithKey f m0

split :: Ord k => k -> NEMap k a -> (M.Map k a, M.Map k a)
split k n@(NEMap k0 v m) = case compare k k0 of
    LT -> (M.empty, toMap n)
    EQ -> (M.empty, m      )
    GT -> first (M.insert k0 v) . M.split k $ m

splitLookup
    :: Ord k
    => k
    -> NEMap k a
    -> (M.Map k a, Maybe a, M.Map k a)
splitLookup k n@(NEMap k0 v m0) = case compare k k0 of
    LT -> (M.empty, Nothing, toMap n)
    EQ -> (M.empty, Just v , m0     )
    GT -> let (m1, x, m2) = M.splitLookup k m0
          in  (M.insert k0 v m1, x, m2)

isSubmapOf :: (Ord k, Eq a) => NEMap k a -> NEMap k a -> Bool
isSubmapOf = isSubmapOfBy (==)

-- performance benefit: can short-circuit, skip an insert
isSubmapOfBy :: Ord k => (a -> b -> Bool) -> NEMap k a -> NEMap k b -> Bool
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
isProperSubmapOfBy :: Ord k => (a -> b -> Bool) -> NEMap k a -> NEMap k b -> Bool
isProperSubmapOfBy f m1 m2 = M.isProperSubmapOfBy f (toMap m1) (toMap m2)

findMin :: NEMap k a -> (k, a)
findMin (NEMap k v _) = (k, v)

findMax :: NEMap k a -> (k, a)
findMax (NEMap k v m) = fromMaybe (k, v) . M.lookupMax $ m

deleteMin :: NEMap k a -> M.Map k a
deleteMin (NEMap _ _ m) = m

-- requires Ord
deleteMax :: Ord k => NEMap k a -> M.Map k a
deleteMax (NEMap k v m) = M.insert k v . M.deleteMax $ m

updateMin :: Ord k => (a -> Maybe a) -> NEMap k a -> M.Map k a
updateMin f = updateMinWithKey (const f)

updateMinWithKey :: Ord k => (k -> a -> Maybe a) -> NEMap k a -> M.Map k a
updateMinWithKey f (NEMap k v m) = ($ m) . maybe id (M.insert k) $ f k v

updateMax :: Ord k => (a -> Maybe a) -> NEMap k a -> M.Map k a
updateMax f = updateMaxWithKey (const f)

updateMaxWithKey :: Ord k => (k -> a -> Maybe a) -> NEMap k a -> M.Map k a
updateMaxWithKey f (NEMap k v m) = M.insert k v . M.updateMaxWithKey f $ m

-- deleteFindMin
minView :: NEMap k a -> (a, M.Map k a)
minView = first snd . deleteFindMin

deleteFindMin :: NEMap k a -> ((k, a), M.Map k a)
deleteFindMin (NEMap k v m) = ((k, v), m)

-- requires Ord
-- deleteFindMax
maxView :: Ord k => NEMap k a -> (a, M.Map k a)
maxView = first snd . deleteFindMax

-- requires Ord
deleteFindMax :: Ord k => NEMap k a -> ((k, a), M.Map k a)
deleteFindMax (NEMap k v m) = maybe ((k, v), M.empty) (second (M.insert k v))
                             . M.maxViewWithKey
                             $ m

valid :: Ord k => NEMap k a -> Bool
valid (NEMap k _ m) = all (k <) (M.keys m) && M.valid m
