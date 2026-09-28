{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.Map.NonEmpty.Internal
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal compatibility module for the lazy non-empty map implementation.
-- Import "Data.Map.NonEmpty.Strict.Internal" for the strict value variant.
module Data.Map.NonEmpty.Internal (
  module Data.Map.NonEmpty.Lazy.Internal,
) where

import Data.Map.NonEmpty.Lazy.Internal
