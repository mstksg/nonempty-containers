{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Containers.NonEmpty.List
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides 'IsNonEmptyList', a non-empty analogue of 'IsList' for the
-- non-empty container types in this package.
module Data.Containers.NonEmpty.List (
  IsNonEmptyList (..),
) where

import qualified Data.IntMap.NonEmpty as NEIM
import Data.IntMap.NonEmpty.Internal (NEIntMap)
import qualified Data.IntSet.NonEmpty as NEIS
import Data.IntSet.NonEmpty.Internal (NEIntSet)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.NonEmpty as NEM
import Data.Map.NonEmpty.Internal (NEMap)
import qualified Data.Semigroup.Foldable as F1
import qualified Data.Sequence.NonEmpty as NESeq
import Data.Sequence.NonEmpty.Internal (NESeq)
import qualified Data.Set.NonEmpty as NES
import Data.Set.NonEmpty.Internal (NESet)
import GHC.Exts (IsList (Item))

-- | Types whose non-empty list representation can be converted to and from
-- the type without losing non-emptiness.
--
-- @since 0.3.6.0
class IsList nes => IsNonEmptyList nes where
  fromNonEmptyList :: NonEmpty (Item nes) -> nes
  toNonEmptyList :: nes -> NonEmpty (Item nes)

-- | @since 0.3.6.0
instance Ord k => IsNonEmptyList (NEMap k a) where
  fromNonEmptyList = NEM.fromList
  toNonEmptyList = NEM.toList

-- | @since 0.3.6.0
instance IsNonEmptyList (NEIntMap a) where
  fromNonEmptyList = NEIM.fromList
  toNonEmptyList = NEIM.toList

-- | @since 0.3.6.0
instance Ord a => IsNonEmptyList (NESet a) where
  fromNonEmptyList = NES.fromList
  toNonEmptyList = NES.toList

-- | @since 0.3.6.0
instance IsNonEmptyList NEIntSet where
  fromNonEmptyList = NEIS.fromList
  toNonEmptyList = NEIS.toList

-- | @since 0.3.6.0
instance IsNonEmptyList (NESeq a) where
  fromNonEmptyList = NESeq.fromList
  toNonEmptyList = F1.toNonEmpty
