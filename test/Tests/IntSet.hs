{-# LANGUAGE TemplateHaskell   #-}

module Tests.IntSet (intSetTests) where

import           Data.Functor.Identity
import           Data.List.NonEmpty            (NonEmpty(..))
import           Data.Semigroup.Foldable
import           Hedgehog
import           Test.Tasty
import           Tests.Util
import qualified Data.IntSet                   as S
import qualified Data.IntSet.NonEmpty          as NES
import qualified Data.IntSet.NonEmpty.Internal as NES
import qualified Data.List.NonEmpty            as NE
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

intSetTests :: TestTree
intSetTests = groupTree $$(discover)





prop_valid :: Property
prop_valid = property $
    assert . NES.valid =<< forAll neIntSetGen


-- | We cannot implement these because there is no 'valid' for IntSet
-- prop_valid_toSet :: Property
-- prop_valid_toSet = property $ do
--     assert . S.valid . NES.toSet =<< forAll neIntSetGen

-- prop_valid_insertMinIntSet :: Property
-- prop_valid_insertMinIntSet = property $ do
--     n  <- forAll $ do
--         m <- setGen
--         let k = maybe dummyKey (subtract 1 . fst) $ S.maxView m
--         pure $ NES.insertMinIntSet k m
--     assert $ S.valid n

-- prop_valid_insertMaxIntSet :: Property
-- prop_valid_insertMaxIntSet = property $ do
--     n  <- forAll $ do
--         m <- setGen
--         let k = maybe dummyKey ((+ 1) . fst) $ S.maxView m
--         pure $ NES.insertMaxIntSet k m
--     assert $ S.valid n

prop_valid_insertSetMin :: Property
prop_valid_insertSetMin = property $ do
    n  <- forAll $ do
        m <- intSetGen
        let k = maybe 0 (subtract 1 . fst) $ S.minView m
        pure $ NES.insertSetMin k m
    assert $ NES.valid n

prop_valid_insertSetMax :: Property
prop_valid_insertSetMax = property $ do
    n  <- forAll $ do
        m <- intSetGen
        let k = maybe 0 ((+ 1) . fst) $ S.maxView m
        pure $ NES.insertSetMax k m
    assert $ NES.valid n

prop_toSetIso1 :: Property
prop_toSetIso1 = property $ do
    m0 <- forAll intSetGen
    tripping m0 NES.nonEmptySet
                (Identity . maybe S.empty NES.toSet)

prop_toSetIso2 :: Property
prop_toSetIso2 = property $ do
    m0 <- forAll $ Gen.maybe neIntSetGen
    tripping m0 (maybe S.empty NES.toSet)
                (Identity . NES.nonEmptySet)

prop_read_show :: Property
prop_read_show = readShow neIntSetGen

prop_splitRoot :: Property
prop_splitRoot = property $ do
    n <- forAll neIntSetGen
    let rs = NES.splitRoot n
        allItems = foldMap1 NES.toList rs
        n' = NES.unions rs
    assert $ ascending allItems
    mapM_ (assert . (`NES.isSubsetOf` n)) rs
    length allItems === NES.size n'
    n === n'
  where
    ascending (x :| xs) = case NE.nonEmpty xs of
      Nothing          -> True
      Just ys@(y :| _) -> x < y && ascending ys










prop_insertSet :: Property
prop_insertSet = ttProp (GTIntKey :-> GTIntSet :-> TTNEIntSet)
    S.insert
    NES.insertSet

prop_singleton :: Property
prop_singleton = ttProp (GTIntKey :-> TTNEIntSet)
    S.singleton
    NES.singleton

prop_fromAscList :: Property
prop_fromAscList = ttProp (GTSorted STAsc (GTNEList Nothing (GTIntKey :&: GTVal)) :-> TTNEIntSet)
    (S.fromAscList   . fmap fst)
    (NES.fromAscList . fmap fst)

prop_fromDistinctAscList :: Property
prop_fromDistinctAscList = ttProp (GTSorted STAsc (GTNEList Nothing GTIntKey) :-> TTNEIntSet)
    S.fromDistinctAscList
    NES.fromDistinctAscList

prop_fromList :: Property
prop_fromList = ttProp (GTNEList Nothing GTIntKey :-> TTNEIntSet)
    S.fromList
    NES.fromList

prop_insert :: Property
prop_insert = ttProp (GTIntKey :-> GTNEIntSet :-> TTNEIntSet)
    S.insert
    NES.insert

prop_delete :: Property
prop_delete = ttProp (GTIntKey :-> GTNEIntSet :-> TTOther)
    S.delete
    NES.delete

prop_member :: Property
prop_member = ttProp (GTIntKey :-> GTNEIntSet :-> TTOther)
    S.member
    NES.member

prop_notMember :: Property
prop_notMember = ttProp (GTIntKey :-> GTNEIntSet :-> TTOther)
    S.notMember
    NES.notMember

prop_lookupLT :: Property
prop_lookupLT = ttProp (GTIntKey :-> GTNEIntSet :-> TTMaybe TTOther)
    S.lookupLT
    NES.lookupLT

prop_lookupGT :: Property
prop_lookupGT = ttProp (GTIntKey :-> GTNEIntSet :-> TTMaybe TTOther)
    S.lookupGT
    NES.lookupGT

prop_lookupLE :: Property
prop_lookupLE = ttProp (GTIntKey :-> GTNEIntSet :-> TTMaybe TTOther)
    S.lookupLE
    NES.lookupLE

prop_lookupGE :: Property
prop_lookupGE = ttProp (GTIntKey :-> GTNEIntSet :-> TTMaybe TTOther)
    S.lookupGE
    NES.lookupGE

prop_size :: Property
prop_size = ttProp (GTNEIntSet :-> TTOther)
    S.size
    NES.size

prop_isSubsetOf :: Property
prop_isSubsetOf = ttProp (GTNEIntSet :-> GTNEIntSet :-> TTOther)
    S.isSubsetOf
    NES.isSubsetOf

prop_isProperSubsetOf :: Property
prop_isProperSubsetOf = ttProp (GTNEIntSet :-> GTNEIntSet :-> TTOther)
    S.isProperSubsetOf
    NES.isProperSubsetOf

prop_disjoint :: Property
prop_disjoint = ttProp (GTNEIntSet :-> GTNEIntSet :-> TTOther)
    NES.disjointSet
    NES.disjoint

prop_union :: Property
prop_union = ttProp (GTNEIntSet :-> GTNEIntSet :-> TTNEIntSet)
    S.union
    NES.union

prop_unions :: Property
prop_unions = ttProp (GTNEList (Just (Range.linear 2 5)) GTNEIntSet :-> TTNEIntSet)
    S.unions
    NES.unions

prop_difference :: Property
prop_difference = ttProp (GTNEIntSet :-> GTNEIntSet :-> TTOther)
    S.difference
    NES.difference

prop_intersection :: Property
prop_intersection = ttProp (GTNEIntSet :-> GTNEIntSet :-> TTOther)
    S.intersection
    NES.intersection

prop_filter :: Property
prop_filter = ttProp (gf1 Gen.bool :?> GTNEIntSet :-> TTOther)
    S.filter
    NES.filter

prop_partition :: Property
prop_partition = ttProp (gf1 Gen.bool :?> GTNEIntSet :-> TTThese TTNEIntSet TTNEIntSet)
    S.partition
    NES.partition

prop_split :: Property
prop_split = ttProp (GTIntKey :-> GTNEIntSet :-> TTMThese TTNEIntSet TTNEIntSet)
    S.split
    NES.split

prop_splitMember :: Property
prop_splitMember = ttProp (GTIntKey :-> GTNEIntSet :-> TTOther :*: TTMThese TTNEIntSet TTNEIntSet)
    (\k -> (\(x,y,z) -> (y,(x,z))) . S.splitMember k)
    NES.splitMember

prop_map :: Property
prop_map = ttProp (gf1 intKeyGen :?> GTNEIntSet :-> TTNEIntSet)
    S.map
    NES.map

prop_foldr :: Property
prop_foldr = ttProp ( gf2 valGen
                  :?> GTOther valGen
                  :-> GTNEIntSet
                  :-> TTOther
                    )
    S.foldr
    NES.foldr

prop_foldl :: Property
prop_foldl = ttProp ( gf2 valGen
                  :?> GTOther valGen
                  :-> GTNEIntSet
                  :-> TTOther
                    )
    S.foldl
    NES.foldl

prop_foldr1 :: Property
prop_foldr1 = ttProp ( gf2 intKeyGen
                   :?> GTNEIntSet
                   :-> TTOther
                     )
    (\f -> foldr1 f . S.toList)
    NES.foldr1

prop_foldl1 :: Property
prop_foldl1 = ttProp ( gf2 intKeyGen
                   :?> GTNEIntSet
                   :-> TTOther
                     )
    (\f -> foldl1 f . S.toList)
    NES.foldl1

prop_foldr' :: Property
prop_foldr' = ttProp ( gf2 intKeyGen
                   :?> GTOther intKeyGen
                   :-> GTNEIntSet
                   :-> TTOther
                     )
    S.foldr'
    NES.foldr'

prop_foldl' :: Property
prop_foldl' = ttProp ( gf2 intKeyGen
                   :?> GTOther intKeyGen
                   :-> GTNEIntSet
                   :-> TTOther
                     )
    S.foldl'
    NES.foldl'

prop_foldr1' :: Property
prop_foldr1' = ttProp ( gf2 intKeyGen
                    :?> GTNEIntSet
                    :-> TTOther
                      )
    (\f -> foldr1 f . S.toList)
    NES.foldr1'

prop_foldl1' :: Property
prop_foldl1' = ttProp ( gf2 intKeyGen
                    :?> GTNEIntSet
                    :-> TTOther
                      )
    (\f -> foldl1 f . S.toList)
    NES.foldl1'

prop_findMin :: Property
prop_findMin = ttProp (GTNEIntSet :-> TTOther)
    S.findMin
    NES.findMin

prop_findMax :: Property
prop_findMax = ttProp (GTNEIntSet :-> TTOther)
    S.findMax
    NES.findMax

prop_deleteMin :: Property
prop_deleteMin = ttProp (GTNEIntSet :-> TTOther)
    S.deleteMin
    NES.deleteMin

prop_deleteMax :: Property
prop_deleteMax = ttProp (GTNEIntSet :-> TTOther)
    S.deleteMax
    NES.deleteMax

prop_deleteFindMin :: Property
prop_deleteFindMin = ttProp (GTNEIntSet :-> TTOther :*: TTOther)
    S.deleteFindMin
    NES.deleteFindMin

prop_deleteFindMax :: Property
prop_deleteFindMax = ttProp (GTNEIntSet :-> TTOther :*: TTOther)
    S.deleteFindMax
    NES.deleteFindMax

prop_toList :: Property
prop_toList = ttProp (GTNEIntSet :-> TTNEList TTOther)
    S.toList
    NES.toList

prop_toDescList :: Property
prop_toDescList = ttProp (GTNEIntSet :-> TTNEList TTOther)
    S.toDescList
    NES.toDescList

