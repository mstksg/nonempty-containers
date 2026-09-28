{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.IntMap.NonEmpty.Strict.Internal
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Strict internal-use functions used in the implementation of
-- "Data.IntMap.NonEmpty.Strict".  These share the same 'NEIntMap' type as
-- the lazy modules; only construction is strict in the value.
module Data.IntMap.NonEmpty.Strict.Internal (
  -- * Non-Empty IntMap type
  NEIntMap,
  pattern NEIntMap,
  neimIntMap,
  Key,
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

  -- * Unsafe IntMap Functions
  insertMinMap,
  insertMaxMap,

  -- * Debug
  valid,
) where

import Control.Applicative
import qualified Data.Foldable as F
import Data.Functor.Apply (Apply)
import Data.IntMap.Internal (IntMap, Key)
import qualified Data.IntMap.NonEmpty.Lazy.Internal as L
import qualified Data.IntMap.Strict as M
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup.Foldable (Foldable1)
import qualified Data.Semigroup.Foldable as F1
import Prelude hiding (Foldable (..), foldl, foldl1, foldr, foldr1, map)

type NEIntMap = L.NEIntMap

pattern NEIntMap :: Key -> a -> IntMap a -> NEIntMap a
pattern NEIntMap k v m <- L.NEIntMap k v m
  where
    NEIntMap k !v m = L.NEIntMap k v m

{-# COMPLETE NEIntMap #-}

neimIntMap :: NEIntMap a -> IntMap a
neimIntMap (NEIntMap _ _ m) = m
{-# INLINE neimIntMap #-}

singleton :: Key -> a -> NEIntMap a
singleton k !v = L.NEIntMap k v M.empty
{-# INLINE singleton #-}

nonEmptyMap :: IntMap a -> Maybe (NEIntMap a)
nonEmptyMap = L.nonEmptyMap
{-# INLINE nonEmptyMap #-}

withNonEmpty :: b -> (NEIntMap a -> b) -> IntMap a -> b
withNonEmpty = L.withNonEmpty
{-# INLINE withNonEmpty #-}

fromList :: NonEmpty (Key, a) -> NEIntMap a
fromList ((k, v) :| xs) = F.foldl' (\m (k', v') -> insertWith const k' v' m) (singleton k v) xs
{-# INLINE fromList #-}

toList :: NEIntMap a -> NonEmpty (Key, a)
toList = L.toList
{-# INLINE toList #-}

map :: (a -> b) -> NEIntMap a -> NEIntMap b
map f (NEIntMap k v m) = NEIntMap k (f v) (M.map f m)
{-# INLINE map #-}

insertWith :: (a -> a -> a) -> Key -> a -> NEIntMap a -> NEIntMap a
insertWith f k !v n@(NEIntMap k0 v0 m) = case compare k k0 of
  LT -> NEIntMap k v (toMap n)
  EQ -> NEIntMap k0 (f v v0) m
  GT -> NEIntMap k0 v0 (M.insertWith f k v m)
{-# INLINE insertWith #-}

union :: NEIntMap a -> NEIntMap a -> NEIntMap a
union n1@(NEIntMap k1 v1 m1) n2@(NEIntMap k2 v2 m2) = case compare k1 k2 of
  LT -> NEIntMap k1 v1 . M.union m1 . toMap $ n2
  EQ -> NEIntMap k1 v1 . M.union m1 $ m2
  GT -> NEIntMap k2 v2 . M.union (toMap n1) $ m2
{-# INLINE union #-}

unions :: Foldable1 f => f (NEIntMap a) -> NEIntMap a
unions ns = case F1.toNonEmpty ns of
  m :| ms -> F.foldl' union m ms
{-# INLINE unions #-}

elems :: NEIntMap a -> NonEmpty a
elems = fmap snd . toList
{-# INLINE elems #-}

size :: NEIntMap a -> Int
size = L.size
{-# INLINE size #-}

toMap :: NEIntMap a -> IntMap a
toMap (NEIntMap k v m) = insertMinMap k v m
{-# INLINE toMap #-}

foldr :: (a -> b -> b) -> b -> NEIntMap a -> b
foldr = L.foldr
{-# INLINE foldr #-}

foldr' :: (a -> b -> b) -> b -> NEIntMap a -> b
foldr' = L.foldr'
{-# INLINE foldr' #-}

foldr1 :: (a -> a -> a) -> NEIntMap a -> a
foldr1 = L.foldr1
{-# INLINE foldr1 #-}

foldl :: (b -> a -> b) -> b -> NEIntMap a -> b
foldl = L.foldl
{-# INLINE foldl #-}

foldl' :: (b -> a -> b) -> b -> NEIntMap a -> b
foldl' = L.foldl'
{-# INLINE foldl' #-}

foldl1 :: (a -> a -> a) -> NEIntMap a -> a
foldl1 = L.foldl1
{-# INLINE foldl1 #-}

traverseWithKey :: Applicative f => (Key -> a -> f b) -> NEIntMap a -> f (NEIntMap b)
traverseWithKey f (NEIntMap k v m) = NEIntMap k <$> f k v <*> M.traverseWithKey f m
{-# INLINE traverseWithKey #-}

traverseWithKey1 :: Apply f => (Key -> a -> f b) -> NEIntMap a -> f (NEIntMap b)
traverseWithKey1 = L.traverseWithKey1
{-# INLINE traverseWithKey1 #-}

foldMapWithKey :: Monoid m => (Key -> a -> m) -> NEIntMap a -> m
foldMapWithKey = L.foldMapWithKey
{-# INLINE foldMapWithKey #-}

valid :: NEIntMap a -> Bool
valid (NEIntMap k _ m) =
  all ((k <) . fst . fst) (M.minViewWithKey m)

insertMinMap :: Key -> a -> IntMap a -> IntMap a
insertMinMap k !v = M.insert k v
{-# INLINE insertMinMap #-}

insertMaxMap :: Key -> a -> IntMap a -> IntMap a
insertMaxMap k !v = M.insert k v
{-# INLINE insertMaxMap #-}
