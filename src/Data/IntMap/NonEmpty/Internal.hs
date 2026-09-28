{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.IntMap.NonEmpty.Internal
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal compatibility module for the lazy non-empty int map
-- implementation.  Import "Data.IntMap.NonEmpty.Strict.Internal" for the
-- strict value variant.
module Data.IntMap.NonEmpty.Internal (
  module Data.IntMap.NonEmpty.Lazy.Internal,
) where

import Data.IntMap.NonEmpty.Lazy.Internal
