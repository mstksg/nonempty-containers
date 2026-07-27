{-# LANGUAGE TemplateHaskell #-}

module Tests.NonEmptyList (nonEmptyListTests) where

import qualified Data.Containers.NonEmpty.List as NEL
import qualified Data.IntMap.NonEmpty as NEIM
import qualified Data.IntSet.NonEmpty as NEIS
import qualified Data.Map.NonEmpty as NEM
import qualified Data.Semigroup.Foldable as F1
import qualified Data.Set.NonEmpty as NES
import Hedgehog
import Test.Tasty
import Tests.Util

nonEmptyListTests :: TestTree
nonEmptyListTests = groupTree $$discover

prop_map_toNonEmptyList :: Property
prop_map_toNonEmptyList =
  property $ do
    m <- forAll neMapGen
    NEL.toNonEmptyList m === NEM.toList m
    NEL.fromNonEmptyList (NEL.toNonEmptyList m) === m

prop_intMap_toNonEmptyList :: Property
prop_intMap_toNonEmptyList =
  property $ do
    m <- forAll neIntMapGen
    NEL.toNonEmptyList m === NEIM.toList m
    NEL.fromNonEmptyList (NEL.toNonEmptyList m) === m

prop_set_toNonEmptyList :: Property
prop_set_toNonEmptyList =
  property $ do
    s <- forAll neSetGen
    NEL.toNonEmptyList s === NES.toList s
    NEL.fromNonEmptyList (NEL.toNonEmptyList s) === s

prop_intSet_toNonEmptyList :: Property
prop_intSet_toNonEmptyList =
  property $ do
    s <- forAll neIntSetGen
    NEL.toNonEmptyList s === NEIS.toList s
    NEL.fromNonEmptyList (NEL.toNonEmptyList s) === s

prop_sequence_toNonEmptyList :: Property
prop_sequence_toNonEmptyList =
  property $ do
    s <- forAll neSeqGen
    NEL.toNonEmptyList s === F1.toNonEmpty s
    NEL.fromNonEmptyList (NEL.toNonEmptyList s) === s
