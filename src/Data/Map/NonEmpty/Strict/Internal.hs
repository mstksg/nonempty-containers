{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.Map.NonEmpty.Strict.Internal
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Strict internal-use functions used in the implementation of
-- "Data.Map.NonEmpty.Strict".  These share the same 'NEMap' type as the
-- lazy modules; only construction is strict in the value.
module Data.Map.NonEmpty.Strict.Internal (
  -- * Non-Empty Map type
  NEMap,
  pattern NEMap,
  nemMap,
  singleton,
  nonEmptyMap,
  withNonEmpty,
  fromList,
  toList,
  map,
  insertWith,
  union,
  unions,
  elems,
  size,
  toMap,

  -- * Folds
  foldr,
  foldr',
  foldr1,
  foldl,
  foldl',
  foldl1,

  -- * Traversals
  traverseWithKey,
  traverseWithKey1,
  foldMapWithKey,

  -- * Unsafe Map Functions
  insertMinMap,
  insertMaxMap,

  -- * Debug
  valid,
) where

import Control.Applicative
import qualified Data.Foldable as F
import Data.Functor.Apply (Apply)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Internal (Map (..))
import qualified Data.Map.Internal as MI
import qualified Data.Map.NonEmpty.Lazy.Internal as L
import qualified Data.Map.Strict as M
import Data.Semigroup.Foldable (Foldable1)
import qualified Data.Semigroup.Foldable as F1
import Prelude hiding (Foldable (..), foldl, foldl1, foldr, foldr1, map)

type NEMap = L.NEMap

pattern NEMap :: k -> a -> Map k a -> NEMap k a
pattern NEMap k v m <- L.NEMap k v m
  where
    NEMap k !v m = L.NEMap k v m

{-# COMPLETE NEMap #-}

nemMap :: NEMap k a -> Map k a
nemMap (NEMap _ _ m) = m
{-# INLINE nemMap #-}

singleton :: k -> a -> NEMap k a
singleton k !v = L.NEMap k v M.empty
{-# INLINE singleton #-}

nonEmptyMap :: Map k a -> Maybe (NEMap k a)
nonEmptyMap = L.nonEmptyMap
{-# INLINE nonEmptyMap #-}

withNonEmpty :: b -> (NEMap k a -> b) -> Map k a -> b
withNonEmpty = L.withNonEmpty
{-# INLINE withNonEmpty #-}

fromList :: Ord k => NonEmpty (k, a) -> NEMap k a
fromList ((k, v) :| xs) = F.foldl' (\m (k', v') -> insertWith const k' v' m) (singleton k v) xs
{-# INLINE fromList #-}

toList :: NEMap k a -> NonEmpty (k, a)
toList = L.toList
{-# INLINE toList #-}

map :: (a -> b) -> NEMap k a -> NEMap k b
map f (NEMap k v m) = NEMap k (f v) (M.map f m)
{-# INLINE map #-}

insertWith :: Ord k => (a -> a -> a) -> k -> a -> NEMap k a -> NEMap k a
insertWith f k !v n@(NEMap k0 v0 m) = case compare k k0 of
  LT -> NEMap k v (toMap n)
  EQ -> NEMap k0 (f v v0) m
  GT -> NEMap k0 v0 (M.insertWith f k v m)
{-# INLINE insertWith #-}

union :: Ord k => NEMap k a -> NEMap k a -> NEMap k a
union n1@(NEMap k1 v1 m1) n2@(NEMap k2 v2 m2) = case compare k1 k2 of
  LT -> NEMap k1 v1 . M.union m1 . toMap $ n2
  EQ -> NEMap k1 v1 . M.union m1 $ m2
  GT -> NEMap k2 v2 . M.union (toMap n1) $ m2
{-# INLINE union #-}

unions :: (Foldable1 f, Ord k) => f (NEMap k a) -> NEMap k a
unions ns = case F1.toNonEmpty ns of
  m :| ms -> F.foldl' union m ms
{-# INLINE unions #-}

elems :: NEMap k a -> NonEmpty a
elems = fmap snd . toList
{-# INLINE elems #-}

size :: NEMap k a -> Int
size = L.size
{-# INLINE size #-}

toMap :: NEMap k a -> Map k a
toMap (NEMap k v m) = insertMinMap k v m
{-# INLINE toMap #-}

foldr :: (a -> b -> b) -> b -> NEMap k a -> b
foldr = L.foldr
{-# INLINE foldr #-}

foldr' :: (a -> b -> b) -> b -> NEMap k a -> b
foldr' = L.foldr'
{-# INLINE foldr' #-}

foldr1 :: (a -> a -> a) -> NEMap k a -> a
foldr1 = L.foldr1
{-# INLINE foldr1 #-}

foldl :: (b -> a -> b) -> b -> NEMap k a -> b
foldl = L.foldl
{-# INLINE foldl #-}

foldl' :: (b -> a -> b) -> b -> NEMap k a -> b
foldl' = L.foldl'
{-# INLINE foldl' #-}

foldl1 :: (a -> a -> a) -> NEMap k a -> a
foldl1 = L.foldl1
{-# INLINE foldl1 #-}

traverseWithKey :: Applicative f => (k -> a -> f b) -> NEMap k a -> f (NEMap k b)
traverseWithKey f (NEMap k v m) = NEMap k <$> f k v <*> M.traverseWithKey f m
{-# INLINE traverseWithKey #-}

traverseWithKey1 :: Apply f => (k -> a -> f b) -> NEMap k a -> f (NEMap k b)
traverseWithKey1 = L.traverseWithKey1
{-# INLINE traverseWithKey1 #-}

foldMapWithKey :: Monoid m => (k -> a -> m) -> NEMap k a -> m
foldMapWithKey = L.foldMapWithKey
{-# INLINE foldMapWithKey #-}

valid :: Ord k => NEMap k a -> Bool
valid (NEMap k _ m) =
  M.valid m
    && all ((k <) . fst . fst) (M.minViewWithKey m)

insertMinMap :: k -> a -> Map k a -> Map k a
insertMinMap kx !x = go
  where
    go Tip = Bin 1 kx x Tip Tip
    go (Bin _ ky y l r) = MI.balanceL ky y (insertMinMap kx x l) r
{-# INLINE insertMinMap #-}

insertMaxMap :: k -> a -> Map k a -> Map k a
insertMaxMap kx !x = go
  where
    go Tip = Bin 1 kx x Tip Tip
    go (Bin _ ky y l r) = MI.balanceR ky y l (insertMaxMap kx x r)
{-# INLINE insertMaxMap #-}
