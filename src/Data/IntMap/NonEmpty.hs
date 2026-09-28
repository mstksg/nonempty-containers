-- |
-- Module      : Data.IntMap.NonEmpty
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- = Non-Empty Finite Integer-Indexed Maps
--
-- This module re-exports "Data.IntMap.NonEmpty.Lazy".  Import
-- "Data.IntMap.NonEmpty.Strict" for the strict value interface.
module Data.IntMap.NonEmpty (
  module Data.IntMap.NonEmpty.Lazy,
) where

import Data.IntMap.NonEmpty.Lazy
