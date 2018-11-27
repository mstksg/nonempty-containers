{-# LANGUAGE TemplateHaskell   #-}

module Tests.Set (setTests) where

import           Data.Functor.Identity
import           Hedgehog
import           Test.Tasty
import           Tests.Util
import qualified Data.Set                   as S
import qualified Data.Set.NonEmpty          as NES
import qualified Data.Set.NonEmpty.Internal as NES
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

setTests :: TestTree
setTests = groupTree $$(discover)





prop_valid :: Property
prop_valid = property $
    assert . NES.valid =<< forAll neSetGen

prop_valid_toSet :: Property
prop_valid_toSet = property $ do
    assert . S.valid . NES.toSet =<< forAll neSetGen

prop_valid_insertMinSet :: Property
prop_valid_insertMinSet = property $ do
    n  <- forAll $ do
        m <- setGen
        let k = maybe dummyKey (subtract 1) $ S.lookupMin m
        pure $ NES.insertMinSet k m
    assert $ S.valid n

prop_valid_insertMaxSet :: Property
prop_valid_insertMaxSet = property $ do
    n  <- forAll $ do
        m <- setGen
        let k = maybe dummyKey (+ 1) $ S.lookupMax m
        pure $ NES.insertMaxSet k m
    assert $ S.valid n

prop_valid_insertSetMin :: Property
prop_valid_insertSetMin = property $ do
    n  <- forAll $ do
        m <- setGen
        let k = maybe dummyKey (subtract 1) $ S.lookupMin m
        pure $ NES.insertSetMin k m
    assert $ NES.valid n

prop_valid_insertSetMax :: Property
prop_valid_insertSetMax = property $ do
    n  <- forAll $ do
        m <- setGen
        let k = maybe dummyKey (+ 1) $ S.lookupMax m
        pure $ NES.insertSetMax k m
    assert $ NES.valid n

prop_toSetIso1 :: Property
prop_toSetIso1 = property $ do
    m0 <- forAll setGen
    tripping m0 NES.nonEmptySet
                (Identity . maybe S.empty NES.toSet)

prop_toSetIso2 :: Property
prop_toSetIso2 = property $ do
    m0 <- forAll $ Gen.maybe neSetGen
    tripping m0 (maybe S.empty NES.toSet)
                (Identity . NES.nonEmptySet)



prop_insertSet :: Property
prop_insertSet = ttProp (GTKey :-> GTSet :-> TTNESet)
    S.insert
    NES.insertSet

prop_singleton :: Property
prop_singleton = ttProp (GTKey :-> TTNESet)
    S.singleton
    NES.singleton

prop_fromAscList :: Property
prop_fromAscList = ttProp (GTSorted STAsc (GTNEList Nothing (GTKey :&: GTVal)) :-> TTNESet)
    (S.fromAscList   . fmap fst)
    (NES.fromAscList . fmap fst)

prop_fromDescList :: Property
prop_fromDescList = ttProp (GTSorted STDesc (GTNEList Nothing (GTKey :&: GTVal)) :-> TTNESet)
    (S.fromDescList   . fmap fst)
    (NES.fromDescList . fmap fst)

prop_fromDistinctAscList :: Property
prop_fromDistinctAscList = ttProp (GTSorted STAsc (GTNEList Nothing GTKey) :-> TTNESet)
    S.fromDistinctAscList
    NES.fromDistinctAscList

prop_fromDistinctDescList :: Property
prop_fromDistinctDescList = ttProp (GTSorted STDesc (GTNEList Nothing GTKey) :-> TTNESet)
    S.fromDistinctDescList
    NES.fromDistinctDescList

prop_fromList :: Property
prop_fromList = ttProp (GTNEList Nothing GTKey :-> TTNESet)
    S.fromList
    NES.fromList

prop_powerSet :: Property
prop_powerSet = ttProp (GTNESet :-> TTNEList TTNESet)
    (S.toList   . S.powerSet  )
    (NES.toList . NES.powerSet)

prop_insert :: Property
prop_insert = ttProp (GTKey :-> GTNESet :-> TTNESet)
    S.insert
    NES.insert

prop_delete :: Property
prop_delete = ttProp (GTKey :-> GTNESet :-> TTSet)
    S.delete
    NES.delete

prop_member :: Property
prop_member = ttProp (GTKey :-> GTNESet :-> TTOther)
    S.member
    NES.member

prop_notMember :: Property
prop_notMember = ttProp (GTKey :-> GTNESet :-> TTOther)
    S.notMember
    NES.notMember

prop_lookupLT :: Property
prop_lookupLT = ttProp (GTKey :-> GTNESet :-> TTMaybe TTKey)
    S.lookupLT
    NES.lookupLT

prop_lookupGT :: Property
prop_lookupGT = ttProp (GTKey :-> GTNESet :-> TTMaybe TTKey)
    S.lookupGT
    NES.lookupGT

prop_lookupLE :: Property
prop_lookupLE = ttProp (GTKey :-> GTNESet :-> TTMaybe TTKey)
    S.lookupLE
    NES.lookupLE

prop_lookupGE :: Property
prop_lookupGE = ttProp (GTKey :-> GTNESet :-> TTMaybe TTKey)
    S.lookupGE
    NES.lookupGE

prop_size :: Property
prop_size = ttProp (GTNESet :-> TTOther)
    S.size
    NES.size

prop_isSubsetOf :: Property
prop_isSubsetOf = ttProp (GTNESet :-> GTNESet :-> TTOther)
    S.isSubsetOf
    NES.isSubsetOf

prop_isProperSubsetOf :: Property
prop_isProperSubsetOf = ttProp (GTNESet :-> GTNESet :-> TTOther)
    S.isProperSubsetOf
    NES.isProperSubsetOf

prop_disjoint :: Property
prop_disjoint = ttProp (GTNESet :-> GTNESet :-> TTOther)
    S.disjoint
    NES.disjoint

prop_union :: Property
prop_union = ttProp (GTNESet :-> GTNESet :-> TTNESet)
    S.union
    NES.union

prop_unions :: Property
prop_unions = ttProp (GTNEList (Just (Range.linear 2 5)) GTNESet :-> TTNESet)
    S.unions
    NES.unions

prop_difference :: Property
prop_difference = ttProp (GTNESet :-> GTNESet :-> TTSet)
    S.difference
    NES.difference

prop_intersection :: Property
prop_intersection = ttProp (GTNESet :-> GTNESet :-> TTSet)
    S.intersection
    NES.intersection

prop_cartesianProduct :: Property
prop_cartesianProduct = ttProp (GTNESet :-> GTNESet :-> TTNEList (TTKey :*: TTKey))
    (\xs -> S.toList   . S.cartesianProduct   xs)
    (\xs -> NES.toList . NES.cartesianProduct xs)

prop_disjointUnion :: Property
prop_disjointUnion = ttProp (GTNESet :-> GTNESet :-> TTNEList (TTEither TTKey TTKey))
    (\xs -> S.toList   . S.disjointUnion   xs)
    (\xs -> NES.toList . NES.disjointUnion xs)

prop_filter :: Property
prop_filter = ttProp (gf1 Gen.bool :?> GTNESet :-> TTSet)
    S.filter
    NES.filter

prop_takeWhileAntitone :: Property
prop_takeWhileAntitone = ttProp (GTNESet :-> TTSet)
    (S.takeWhileAntitone   ((< 0) . getKX))
    (NES.takeWhileAntitone ((< 0) . getKX))

prop_dropWhileAntitone :: Property
prop_dropWhileAntitone = ttProp (GTNESet :-> TTSet)
    (S.dropWhileAntitone   ((< 0) . getKX))
    (NES.dropWhileAntitone ((< 0) . getKX))

prop_spanAntitone :: Property
prop_spanAntitone = ttProp (GTNESet :-> TTThese TTNESet TTNESet)
    (S.spanAntitone   ((< 0) . getKX))
    (NES.spanAntitone ((< 0) . getKX))

prop_partition :: Property
prop_partition = ttProp (gf1 Gen.bool :?> GTNESet :-> TTThese TTNESet TTNESet)
    S.partition
    NES.partition

prop_split :: Property
prop_split = ttProp (GTKey :-> GTNESet :-> TTMThese TTNESet TTNESet)
    S.split
    NES.split

prop_splitMember :: Property
prop_splitMember = ttProp (GTKey :-> GTNESet :-> TTOther :*: TTMThese TTNESet TTNESet)
    (\k -> (\(x,y,z) -> (y,(x,z))) . S.splitMember k)
    NES.splitMember

  -- , splitRoot

prop_lookupIndex :: Property
prop_lookupIndex = ttProp (GTKey :-> GTNESet :-> TTMaybe TTOther)
    S.lookupIndex
    NES.lookupIndex

  -- , findIndex
  -- , elemAt
  -- , deleteAt

prop_take :: Property
prop_take = ttProp (GTOther (Gen.int mapSize) :-> GTNESet :-> TTSet)
    S.take
    NES.take

prop_drop :: Property
prop_drop = ttProp (GTOther (Gen.int mapSize) :-> GTNESet :-> TTSet)
    S.drop
    NES.drop

prop_splitAt :: Property
prop_splitAt = ttProp (GTOther (Gen.int mapSize) :-> GTNESet :-> TTThese TTNESet TTNESet)
    S.splitAt
    NES.splitAt

prop_map :: Property
prop_map = ttProp (gf1 keyGen :?> GTNESet :-> TTNESet)
    S.map
    NES.map

prop_mapKeysMonotonic :: Property
prop_mapKeysMonotonic = ttProp (GF valGen go :?> GTNESet :-> TTNESet)
    S.mapMonotonic
    NES.mapMonotonic
  where
    go f (K i t) = K (i * 2) (f t)

prop_foldr :: Property
prop_foldr = ttProp ( gf2 valGen
                  :?> GTOther valGen
                  :-> GTNESet
                  :-> TTOther
                    )
    S.foldr
    NES.foldr

prop_foldl :: Property
prop_foldl = ttProp ( gf2 valGen
                  :?> GTOther valGen
                  :-> GTNESet
                  :-> TTOther
                    )
    S.foldl
    NES.foldl

prop_foldr1 :: Property
prop_foldr1 = ttProp ( gf2 keyGen
                   :?> GTNESet
                   :-> TTOther
                     )
    foldr1
    NES.foldr1

prop_foldl1 :: Property
prop_foldl1 = ttProp ( gf2 keyGen
                   :?> GTNESet
                   :-> TTOther
                     )
    foldl1
    NES.foldl1

prop_foldr' :: Property
prop_foldr' = ttProp ( gf2 keyGen
                   :?> GTOther keyGen
                   :-> GTNESet
                   :-> TTOther
                     )
    S.foldr'
    NES.foldr'

prop_foldl' :: Property
prop_foldl' = ttProp ( gf2 keyGen
                   :?> GTOther keyGen
                   :-> GTNESet
                   :-> TTOther
                     )
    S.foldl'
    NES.foldl'

prop_foldr1' :: Property
prop_foldr1' = ttProp ( gf2 keyGen
                    :?> GTNESet
                    :-> TTOther
                      )
    foldr1
    NES.foldr1'

prop_foldl1' :: Property
prop_foldl1' = ttProp ( gf2 keyGen
                    :?> GTNESet
                    :-> TTOther
                      )
    foldl1
    NES.foldl1'

prop_findMin :: Property
prop_findMin = ttProp (GTNESet :-> TTKey)
    S.findMin
    NES.findMin

prop_findMax :: Property
prop_findMax = ttProp (GTNESet :-> TTKey)
    S.findMax
    NES.findMax

prop_deleteMin :: Property
prop_deleteMin = ttProp (GTNESet :-> TTSet)
    S.deleteMin
    NES.deleteMin

prop_deleteMax :: Property
prop_deleteMax = ttProp (GTNESet :-> TTSet)
    S.deleteMax
    NES.deleteMax

prop_deleteFindMin :: Property
prop_deleteFindMin = ttProp (GTNESet :-> TTKey :*: TTSet)
    S.deleteFindMin
    NES.deleteFindMin

prop_deleteFindMax :: Property
prop_deleteFindMax = ttProp (GTNESet :-> TTKey :*: TTSet)
    S.deleteFindMax
    NES.deleteFindMax

prop_toList :: Property
prop_toList = ttProp (GTNESet :-> TTNEList TTKey)
    S.toList
    NES.toList

prop_toDescList :: Property
prop_toDescList = ttProp (GTNESet :-> TTNEList TTKey)
    S.toDescList
    NES.toDescList
