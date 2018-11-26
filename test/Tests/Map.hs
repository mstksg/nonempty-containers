{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Tests.Map (mapTests) where

import           Data.Foldable
import           Data.Functor.Identity
import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import           Hedgehog
import           Tests.Map.Util
import qualified Data.Map                   as M
import qualified Data.Map.NonEmpty          as NEM
import qualified Data.Map.NonEmpty.Internal as NEM
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

mapTests :: Group
mapTests = $$(discover)


dummyKey :: KeyType
dummyKey = K 0 "hello"





prop_valid :: Property
prop_valid = property $
    assert . NEM.valid =<< forAll neMapGen

prop_valid_toMap :: Property
prop_valid_toMap = property $ do
    assert . M.valid . NEM.toMap =<< forAll neMapGen

prop_valid_insertMinMap :: Property
prop_valid_insertMinMap = property $ do
    n  <- forAll $ do
        m <- mapGen
        let k = maybe dummyKey (subtract 1 . fst) $ M.lookupMin m
        v <- valGen
        pure $ NEM.insertMinMap k v m
    assert $ M.valid n

prop_valid_insertMaxMap :: Property
prop_valid_insertMaxMap = property $ do
    n  <- forAll $ do
        m <- mapGen
        let k = maybe dummyKey ((+ 1) . fst) $ M.lookupMax m
        v <- valGen
        pure $ NEM.insertMaxMap k v m
    assert $ M.valid n

prop_valid_insertMapMin :: Property
prop_valid_insertMapMin = property $ do
    n  <- forAll $ do
        m <- mapGen
        let k = maybe dummyKey (subtract 1 . fst) $ M.lookupMin m
        v <- valGen
        pure $ NEM.insertMapMin k v m
    assert $ NEM.valid n

prop_valid_insertMapMax :: Property
prop_valid_insertMapMax = property $ do
    n  <- forAll $ do
        m <- mapGen
        let k = maybe dummyKey ((+ 1) . fst) $ M.lookupMax m
        v <- valGen
        pure $ NEM.insertMapMax k v m
    assert $ NEM.valid n

prop_toMapIso1 :: Property
prop_toMapIso1 = property $ do
    m0 <- forAll mapGen
    tripping m0 NEM.nonEmptyMap
                (Identity . maybe M.empty NEM.toMap)

prop_toMapIso2 :: Property
prop_toMapIso2 = property $ do
    m0 <- forAll $ Gen.maybe neMapGen
    tripping m0 (maybe M.empty NEM.toMap)
                (Identity . NEM.nonEmptyMap)

prop_insertMapWithKey :: Property
prop_insertMapWithKey = ttProp (gf3 valGen :?> GTKey :-> GTVal :-> GTNEMap :-> TTNEMap)
    M.insertWithKey
    NEM.insertWithKey

prop_singleton :: Property
prop_singleton = ttProp (GTKey :-> GTVal :-> TTNEMap)
    M.singleton
    NEM.singleton

prop_fromAscList :: Property
prop_fromAscList = ttProp (GTSorted STAsc (GTNEList Nothing (GTKey :&: GTVal)) :-> TTNEMap)
    M.fromAscList
    NEM.fromAscList

prop_fromDescList :: Property
prop_fromDescList = ttProp (GTSorted STDesc (GTNEList Nothing (GTKey :&: GTVal)) :-> TTNEMap)
    M.fromDescList
    NEM.fromDescList

prop_fromAscListWithKey :: Property
prop_fromAscListWithKey = ttProp (gf3 valGen :?> GTSorted STAsc (GTNEList Nothing (GTKey :&: GTVal)) :-> TTNEMap)
    M.fromAscListWithKey
    NEM.fromAscListWithKey

prop_fromDescListWithKey :: Property
prop_fromDescListWithKey = ttProp (gf3 valGen :?> GTSorted STDesc (GTNEList Nothing (GTKey :&: GTVal)) :-> TTNEMap)
    M.fromDescListWithKey
    NEM.fromDescListWithKey

prop_fromDistinctAscList :: Property
prop_fromDistinctAscList = ttProp (GTSorted STDistinctAsc (GTNEList Nothing (GTKey :&: GTVal)) :-> TTNEMap)
    M.fromDistinctAscList
    NEM.fromDistinctAscList

prop_fromDistinctDescList :: Property
prop_fromDistinctDescList = ttProp (GTSorted STDistinctDesc (GTNEList Nothing (GTKey :&: GTVal)) :-> TTNEMap)
    M.fromDistinctDescList
    NEM.fromDistinctDescList

prop_fromListWithKey :: Property
prop_fromListWithKey = ttProp (gf3 valGen :?> GTNEList Nothing (GTKey :&: GTVal) :-> TTNEMap)
    M.fromListWithKey
    NEM.fromListWithKey

prop_insert :: Property
prop_insert = ttProp (GTKey :-> GTVal :-> GTNEMap :-> TTNEMap)
    M.insert
    NEM.insert

prop_insertWithKey :: Property
prop_insertWithKey = ttProp (gf3 valGen :?> GTKey :-> GTVal :-> GTNEMap :-> TTNEMap)
    M.insertWithKey
    NEM.insertWithKey

prop_delete :: Property
prop_delete = ttProp (GTKey :-> GTNEMap :-> TTMap)
    M.delete
    NEM.delete

prop_adjustWithKey :: Property
prop_adjustWithKey = ttProp (gf2 valGen :?> GTKey :-> GTNEMap :-> TTNEMap)
    M.adjustWithKey
    NEM.adjustWithKey

prop_updateWithKey :: Property
prop_updateWithKey = ttProp (gf2 (Gen.maybe valGen) :?> GTKey :-> GTNEMap :-> TTMap)
    M.updateWithKey
    NEM.updateWithKey

prop_updateLookupWithKey :: Property
prop_updateLookupWithKey = ttProp (gf2 (Gen.maybe valGen) :?> GTKey :-> GTNEMap :-> TTMaybe TTVal :*: TTMap)
    M.updateLookupWithKey
    NEM.updateLookupWithKey

prop_alter :: Property
prop_alter = ttProp (gf1 (Gen.maybe valGen) :?> GTKey :-> GTNEMap :-> TTMap)
    M.alter
    NEM.alter

prop_alter' :: Property
prop_alter' = ttProp (gf1 valGen :?> GTKey :-> GTNEMap :-> TTNEMap)
    (M.alter . fmap Just)
    NEM.alter'

prop_alterF :: Property
prop_alterF = ttProp (GTKey :-> GTNEMap :-> TTCtx (GTMaybe GTVal :-> TTMap) (TTMaybe TTVal))
    (M.alterF   (Context id))
    (NEM.alterF (Context id))

prop_alterF' :: Property
prop_alterF' = ttProp (GTKey :-> GTNEMap :-> TTCtx (GTVal :-> TTNEMap) (TTMaybe TTVal))
    (M.alterF    (Context Just))
    (NEM.alterF' (Context id))

prop_lookup :: Property
prop_lookup = ttProp (GTKey :-> GTNEMap :-> TTMaybe TTVal)
    M.lookup
    NEM.lookup

prop_findWithDefault :: Property
prop_findWithDefault = ttProp (GTVal :-> GTKey :-> GTNEMap :-> TTVal)
    M.findWithDefault
    NEM.findWithDefault

prop_member :: Property
prop_member = ttProp (GTKey :-> GTNEMap :-> TTOther)
    M.member
    NEM.member

prop_notMember :: Property
prop_notMember = ttProp (GTKey :-> GTNEMap :-> TTOther)
    M.notMember
    NEM.notMember

prop_lookupLT :: Property
prop_lookupLT = ttProp (GTKey :-> GTNEMap :-> TTMaybe (TTKey :*: TTVal))
    M.lookupLT
    NEM.lookupLT

prop_lookupGT :: Property
prop_lookupGT = ttProp (GTKey :-> GTNEMap :-> TTMaybe (TTKey :*: TTVal))
    M.lookupGT
    NEM.lookupGT

prop_lookupLE :: Property
prop_lookupLE = ttProp (GTKey :-> GTNEMap :-> TTMaybe (TTKey :*: TTVal))
    M.lookupLE
    NEM.lookupLE

prop_lookupGE :: Property
prop_lookupGE = ttProp (GTKey :-> GTNEMap :-> TTMaybe (TTKey :*: TTVal))
    M.lookupGE
    NEM.lookupGE

prop_size :: Property
prop_size = ttProp (GTNEMap :-> TTOther)
    M.size
    NEM.size

prop_union :: Property
prop_union = ttProp (GTNEMap :-> GTNEMap :-> TTNEMap)
    M.union
    NEM.union

prop_unionWith :: Property
prop_unionWith = ttProp (gf2 valGen :?> GTNEMap :-> GTNEMap :-> TTNEMap)
    M.unionWith
    NEM.unionWith

prop_unionWithKey :: Property
prop_unionWithKey = ttProp (gf3 valGen :?> GTNEMap :-> GTNEMap :-> TTNEMap)
    M.unionWithKey
    NEM.unionWithKey

prop_unions :: Property
prop_unions = ttProp (GTNEList (Just (Range.linear 2 5)) GTNEMap :-> TTNEMap)
    M.unions
    NEM.unions

prop_unionsWith :: Property
prop_unionsWith = ttProp (gf2 valGen :?> GTNEList (Just (Range.linear 2 5)) GTNEMap :-> TTNEMap)
    M.unionsWith
    NEM.unionsWith

prop_difference :: Property
prop_difference = ttProp (GTNEMap :-> GTNEMap :-> TTMap)
    M.difference
    NEM.difference

prop_differenceWithKey :: Property
prop_differenceWithKey = ttProp (gf3 (Gen.maybe valGen) :?> GTNEMap :-> GTNEMap :-> TTMap)
    M.differenceWithKey
    NEM.differenceWithKey

prop_intersection :: Property
prop_intersection = ttProp (GTNEMap :-> GTNEMap :-> TTMap)
    M.intersection
    NEM.intersection

prop_intersectionWithKey :: Property
prop_intersectionWithKey = ttProp (gf3 valGen :?> GTNEMap :-> GTNEMap :-> TTMap)
    M.intersectionWithKey
    NEM.intersectionWithKey

prop_map :: Property
prop_map = ttProp (gf1 valGen :?> GTNEMap :-> TTNEMap)
    M.map
    NEM.map

prop_mapWithKey :: Property
prop_mapWithKey = ttProp (gf2 valGen :?> GTNEMap :-> TTNEMap)
    M.mapWithKey
    NEM.mapWithKey

prop_traverseWithKey1 :: Property
prop_traverseWithKey1 = ttProp (GTNEMap :-> TTBazaar GTVal TTNEMap TTVal)
    (M.traverseWithKey    (\k -> (`More` Done (k,))))
    (NEM.traverseWithKey1 (\k -> (`More` Done (k,))))

prop_traverseWithKey :: Property
prop_traverseWithKey = ttProp (GTNEMap :-> TTBazaar GTVal TTNEMap TTVal)
    (M.traverseWithKey   (\k -> (`More` Done (k,))))
    (NEM.traverseWithKey (\k -> (`More` Done (k,))))

prop_traverseMaybeWithKey1 :: Property
prop_traverseMaybeWithKey1 = ttProp (GTNEMap :-> TTBazaar (GTMaybe GTVal) TTMap TTVal)
    (M.traverseMaybeWithKey    (\k -> (`More` Done (fmap (k,)))))
    (NEM.traverseMaybeWithKey1 (\k -> (`More` Done (fmap (k,)))))

prop_traverseMaybeWithKey :: Property
prop_traverseMaybeWithKey = ttProp (GTNEMap :-> TTBazaar (GTMaybe GTVal) TTMap TTVal)
    (M.traverseMaybeWithKey   (\k -> (`More` Done (fmap (k,)))))
    (NEM.traverseMaybeWithKey (\k -> (`More` Done (fmap (k,)))))

prop_sequence1 :: Property
prop_sequence1 = ttProp (GTNEMap :-> TTBazaar GTVal TTNEMap TTVal)
    (sequenceA . fmap (`More` Done id))
    (sequence1 . fmap (`More` Done id))

prop_sequenceA :: Property
prop_sequenceA = ttProp (GTNEMap :-> TTBazaar GTVal TTNEMap TTVal)
    (sequenceA . fmap (`More` Done id))
    (sequenceA . fmap (`More` Done id))

prop_mapAccumWithKey :: Property
prop_mapAccumWithKey = ttProp  ( gf3 ((,) <$> valGen <*> valGen)
                             :?> GTOther valGen
                             :-> GTNEMap
                             :-> TTOther :*: TTNEMap
                               )
    M.mapAccumWithKey
    NEM.mapAccumWithKey

prop_mapAccumRWithKey :: Property
prop_mapAccumRWithKey = ttProp  ( gf3 ((,) <$> valGen <*> valGen)
                              :?> GTOther valGen
                              :-> GTNEMap
                              :-> TTOther :*: TTNEMap
                                )
    M.mapAccumRWithKey
    NEM.mapAccumRWithKey

prop_mapKeys :: Property
prop_mapKeys = ttProp (gf1 keyGen :?> GTNEMap :-> TTNEMap)
    M.mapKeys
    NEM.mapKeys
  
prop_mapKeysWith :: Property
prop_mapKeysWith = ttProp ( gf2 valGen
                        :?> gf1 keyGen
                        :?> GTNEMap
                        :-> TTNEMap
                          )
    M.mapKeysWith
    NEM.mapKeysWith

prop_mapMonotonic :: Property
prop_mapMonotonic = ttProp (gf1 keyGen :?> GTNEMap :-> TTNEMap)
    M.mapKeysMonotonic
    NEM.mapKeysMonotonic

prop_foldr :: Property
prop_foldr = ttProp ( gf2 valGen
                  :?> GTOther valGen
                  :-> GTNEMap
                  :-> TTOther
                    )
    M.foldr
    NEM.foldr
  
prop_foldl :: Property
prop_foldl = ttProp ( gf2 valGen
                  :?> GTOther valGen
                  :-> GTNEMap
                  :-> TTOther
                    )
    M.foldl
    NEM.foldl

prop_foldr1 :: Property
prop_foldr1 = ttProp ( gf2 valGen
                   :?> GTNEMap
                   :-> TTOther
                     )
    foldr1
    NEM.foldr1
  
prop_foldl1 :: Property
prop_foldl1 = ttProp ( gf2 valGen
                   :?> GTNEMap
                   :-> TTOther
                     )
    foldl1
    NEM.foldl1
  
prop_foldrWithKey :: Property
prop_foldrWithKey = ttProp ( gf3 valGen
                         :?> GTOther valGen
                         :-> GTNEMap
                         :-> TTOther
                           )
    M.foldrWithKey
    NEM.foldrWithKey
  
prop_foldlWithKey :: Property
prop_foldlWithKey = ttProp ( gf3 valGen
                         :?> GTOther valGen
                         :-> GTNEMap
                         :-> TTOther
                           )
    M.foldlWithKey
    NEM.foldlWithKey
  
prop_foldMapWithKey :: Property
prop_foldMapWithKey = ttProp (gf2 valGen :?> GTNEMap :-> TTOther)
    M.foldMapWithKey
    NEM.foldMapWithKey
  
prop_foldr' :: Property
prop_foldr' = ttProp ( gf2 valGen
                   :?> GTOther valGen
                   :-> GTNEMap
                   :-> TTOther
                     )
    M.foldr'
    NEM.foldr'
  
prop_foldl' :: Property
prop_foldl' = ttProp ( gf2 valGen
                   :?> GTOther valGen
                   :-> GTNEMap
                   :-> TTOther
                     )
    M.foldl'
    NEM.foldl'

prop_foldr1' :: Property
prop_foldr1' = ttProp ( gf2 valGen
                    :?> GTNEMap
                    :-> TTOther
                      )
    foldr1
    NEM.foldr1'
  
prop_foldl1' :: Property
prop_foldl1' = ttProp ( gf2 valGen
                    :?> GTNEMap
                    :-> TTOther
                      )
    foldl1
    NEM.foldl1'
  
prop_foldrWithKey' :: Property
prop_foldrWithKey' = ttProp ( gf3 valGen
                          :?> GTOther valGen
                          :-> GTNEMap
                          :-> TTOther
                            )
    M.foldrWithKey'
    NEM.foldrWithKey'
  
prop_foldlWithKey' :: Property
prop_foldlWithKey' = ttProp ( gf3 valGen
                          :?> GTOther valGen
                          :-> GTNEMap
                          :-> TTOther
                            )
    M.foldlWithKey'
    NEM.foldlWithKey'

prop_elems :: Property
prop_elems = ttProp (GTNEMap :-> TTNEList TTVal)
    M.elems
    NEM.elems

prop_keys :: Property
prop_keys = ttProp (GTNEMap :-> TTNEList TTKey)
    M.keys
    NEM.keys

prop_assocs :: Property
prop_assocs = ttProp (GTNEMap :-> TTNEList (TTKey :*: TTVal))
    M.assocs
    NEM.assocs

prop_toList :: Property
prop_toList = ttProp (GTNEMap :-> TTNEList (TTKey :*: TTVal))
    M.toList
    NEM.toList

prop_toDescList :: Property
prop_toDescList = ttProp (GTNEMap :-> TTNEList (TTKey :*: TTVal))
    M.toDescList
    NEM.toDescList

prop_filter :: Property
prop_filter = ttProp (gf1 Gen.bool :?> GTNEMap :-> TTMap)
    M.filter
    NEM.filter

prop_filterWithKey :: Property
prop_filterWithKey = ttProp (gf2 Gen.bool :?> GTNEMap :-> TTMap)
    M.filterWithKey
    NEM.filterWithKey

prop_restrictKeys :: Property
prop_restrictKeys = ttProp (GTNEMap :-> GTSet GTKey :-> TTMap)
    M.restrictKeys
    NEM.restrictKeys

prop_withoutKeys :: Property
prop_withoutKeys = ttProp (GTNEMap :-> GTSet GTKey :-> TTMap)
    M.withoutKeys
    NEM.withoutKeys

prop_partitionWithKey :: Property
prop_partitionWithKey = ttProp (gf2 Gen.bool :?> GTNEMap :-> TTThese TTNEMap TTNEMap)
    M.partitionWithKey
    NEM.partitionWithKey
    
prop_takeWhileAntitone :: Property
prop_takeWhileAntitone = ttProp (GTNEMap :-> TTMap)
    (M.takeWhileAntitone   ((< 0) . getKX))
    (NEM.takeWhileAntitone ((< 0) . getKX))

prop_dropWhileAntitone :: Property
prop_dropWhileAntitone = ttProp (GTNEMap :-> TTMap)
    (M.dropWhileAntitone   ((< 0) . getKX))
    (NEM.dropWhileAntitone ((< 0) . getKX))

prop_spanAntitone :: Property
prop_spanAntitone = ttProp (GTNEMap :-> TTThese TTNEMap TTNEMap)
    (M.spanAntitone   ((< 0) . getKX))
    (NEM.spanAntitone ((< 0) . getKX))

prop_mapMaybeWithKey :: Property
prop_mapMaybeWithKey = ttProp (gf2 (Gen.maybe valGen) :?> GTNEMap :-> TTMap)
    M.mapMaybeWithKey
    NEM.mapMaybeWithKey

prop_mapEitherWithKey :: Property
prop_mapEitherWithKey = ttProp ( gf2 (Gen.choice [Left <$> valGen, Right <$> valGen])
                             :?> GTNEMap
                             :-> TTThese TTNEMap TTNEMap
                               )
    M.mapEitherWithKey
    NEM.mapEitherWithKey

prop_split :: Property
prop_split = ttProp (GTKey :-> GTNEMap :-> TTMThese TTNEMap TTNEMap)
    M.split
    NEM.split

prop_splitLookup :: Property
prop_splitLookup = ttProp (GTKey :-> GTNEMap :-> TTMaybe TTVal :*: TTMThese TTNEMap TTNEMap)
    (\k -> (\(x,y,z) -> (y,(x,z))) . M.splitLookup k)
    NEM.splitLookup

prop_isSubmapOfBy :: Property
prop_isSubmapOfBy = ttProp (gf2 Gen.bool :?> GTNEMap :-> GTNEMap :-> TTOther)
    M.isSubmapOfBy
    NEM.isSubmapOfBy

prop_isProperSubmapOfBy :: Property
prop_isProperSubmapOfBy = ttProp (gf2 Gen.bool :?> GTNEMap :-> GTNEMap :-> TTOther)
    M.isProperSubmapOfBy
    NEM.isProperSubmapOfBy

prop_lookupIndex :: Property
prop_lookupIndex = ttProp (GTKey :-> GTNEMap :-> TTMaybe TTOther)
    M.lookupIndex
    NEM.lookupIndex

  -- , adjustAt
  -- , updateAt
  -- , deleteAt

prop_take :: Property
prop_take = ttProp (GTOther (Gen.int mapSize) :-> GTNEMap :-> TTMap)
    M.take
    NEM.take

prop_drop :: Property
prop_drop = ttProp (GTOther (Gen.int mapSize) :-> GTNEMap :-> TTMap)
    M.drop
    NEM.drop

prop_splitAt :: Property
prop_splitAt = ttProp (GTOther (Gen.int mapSize) :-> GTNEMap :-> TTThese TTNEMap TTNEMap)
    M.splitAt
    NEM.splitAt

prop_findMin :: Property
prop_findMin = ttProp (GTNEMap :-> TTKey :*: TTVal)
    M.findMin
    NEM.findMin

prop_findMax :: Property
prop_findMax = ttProp (GTNEMap :-> TTKey :*: TTVal)
    M.findMax
    NEM.findMax

prop_deleteMin :: Property
prop_deleteMin = ttProp (GTNEMap :-> TTMap)
    M.deleteMin
    NEM.deleteMin

prop_deleteMax :: Property
prop_deleteMax = ttProp (GTNEMap :-> TTMap)
    M.deleteMax
    NEM.deleteMax

prop_deleteFindMin :: Property
prop_deleteFindMin = ttProp (GTNEMap :-> (TTKey :*: TTVal) :*: TTMap)
    M.deleteFindMin
    NEM.deleteFindMin

prop_deleteFindMax :: Property
prop_deleteFindMax = ttProp (GTNEMap :-> (TTKey :*: TTVal) :*: TTMap)
    M.deleteFindMax
    NEM.deleteFindMax

prop_updateMinWithKey :: Property
prop_updateMinWithKey = ttProp (gf2 (Gen.maybe valGen) :?> GTNEMap :-> TTMap)
    M.updateMinWithKey
    NEM.updateMinWithKey

prop_updateMaxWithKey :: Property
prop_updateMaxWithKey = ttProp (gf2 (Gen.maybe valGen) :?> GTNEMap :-> TTMap)
    M.updateMaxWithKey
    NEM.updateMaxWithKey

prop_adjustMinWithKey :: Property
prop_adjustMinWithKey = ttProp (gf2 valGen :?> GTNEMap :-> TTNEMap)
    (M.updateMinWithKey  . (fmap . fmap) Just)
    NEM.adjustMinWithKey

prop_adjustMaxWithKey :: Property
prop_adjustMaxWithKey = ttProp (gf2 valGen :?> GTNEMap :-> TTNEMap)
    (M.updateMaxWithKey  . (fmap . fmap) Just)
    NEM.adjustMaxWithKey

prop_minView :: Property
prop_minView = ttProp (GTNEMap :-> TTMaybe (TTVal :*: TTMap))
    M.minView
    (Just . NEM.minView)

prop_maxView :: Property
prop_maxView = ttProp (GTNEMap :-> TTMaybe (TTVal :*: TTMap))
    M.maxView
    (Just . NEM.maxView)

prop_elem :: Property
prop_elem = ttProp (GTVal :-> GTNEMap :-> TTOther)
    elem
    elem

prop_fold1 :: Property
prop_fold1 = ttProp (GTNEMap :-> TTVal)
    fold
    fold1

prop_fold :: Property
prop_fold = ttProp (GTNEMap :-> TTVal)
    fold
    fold

prop_foldMap1 :: Property
prop_foldMap1 = ttProp (GTNEMap :-> TTOther)
    (foldMap  (:[]))
    (foldMap1 (:[]))

prop_foldMap :: Property
prop_foldMap = ttProp (GTNEMap :-> TTOther)
    (foldMap (:[]))
    (foldMap (:[]))

