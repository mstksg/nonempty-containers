-- |
-- Module      : Data.Map.NonEmpty
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- = Non-Empty Finite Maps
--
-- This module re-exports "Data.Map.NonEmpty.Lazy".  Import
-- "Data.Map.NonEmpty.Strict" for the strict value interface.
module Data.Map.NonEmpty (
  module Data.Map.NonEmpty.Lazy,
) where

import Data.Map.NonEmpty.Lazy
