{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Tests.IntMap (intMapTests) where

import           Control.Applicative
import           Control.Comonad
import           Data.Coerce
import           Data.Foldable
import           Data.Functor.Alt
import           Data.Functor.Identity
import           Data.List.NonEmpty            (NonEmpty(..))
import           Data.Semigroup.Foldable
import           Data.Text                     (Text)
import           Hedgehog
import           Test.Tasty
import           Tests.Util
import qualified Data.IntMap                   as M
import qualified Data.IntMap.NonEmpty          as NEM
import qualified Data.IntMap.NonEmpty.Internal as NEM
import qualified Data.List.NonEmpty            as NE
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

intMapTests :: TestTree
intMapTests = groupTree $$(discover)





prop_valid :: Property
prop_valid = property $
    assert . NEM.valid =<< forAll neIntMapGen

-- | We cannot implement these because there is no 'valid' for IntSet
-- prop_valid_toMap :: Property
-- prop_valid_toMap = property $
--     assert . M.valid . NEM.toMap =<< forAll neIntMapGen

-- prop_valid_insertMinIntMap :: Property
-- prop_valid_insertMinIntMap = property $ do
--     n  <- forAll $ do
--         m <- intMapGen
--         let k = maybe 0 (subtract 1 . fst) $ M.lookupMin m
--         v <- valGen
--         pure $ NEM.insertMinIntMap k v m
--     assert $ M.valid n

-- prop_valid_insertMaxIntMap :: Property
-- prop_valid_insertMaxIntMap = property $ do
--     n  <- forAll $ do
--         m <- intMapGen
--         let k = maybe 0 ((+ 1) . fst) $ M.lookupMax m
--         v <- valGen
--         pure $ NEM.insertMaxIntMap k v m
--     assert $ M.valid n

prop_valid_insertMapMin :: Property
prop_valid_insertMapMin = property $ do
    n  <- forAll $ do
        m <- intMapGen
        let k = maybe 0 (subtract 1 . fst) $ NEM.lookupMinMap m
        v <- valGen
        pure $ NEM.insertMapMin k v m
    assert $ NEM.valid n

prop_valid_insertMapMax :: Property
prop_valid_insertMapMax = property $ do
    n  <- forAll $ do
        m <- intMapGen
        let k = maybe 0 ((+ 1) . fst) $ NEM.lookupMaxMap m
        v <- valGen
        pure $ NEM.insertMapMax k v m
    assert $ NEM.valid n

prop_toMapIso1 :: Property
prop_toMapIso1 = property $ do
    m0 <- forAll intMapGen
    tripping m0 (NEM.nonEmptyMap)
                (Identity . maybe M.empty NEM.toMap)

prop_toMapIso2 :: Property
prop_toMapIso2 = property $ do
    m0 <- forAll $ Gen.maybe neIntMapGen
    tripping m0 (maybe M.empty NEM.toMap)
                (Identity . NEM.nonEmptyMap)

prop_read_show :: Property
prop_read_show = readShow neIntMapGen

prop_read1_show1 :: Property
prop_read1_show1 = readShow1 neIntMapGen

prop_show_show1 :: Property
prop_show_show1 = showShow1 neIntMapGen

prop_splitRoot :: Property
prop_splitRoot = property $ do
    n <- forAll neIntMapGen
    let rs = NEM.splitRoot n
        allItems = foldMap1 NEM.keys rs
        n' = NEM.unions rs
    assert $ ascending allItems
    mapM_ (assert . (`NEM.isSubmapOf` n)) rs
    length allItems === length n'
    n === n'
  where
    ascending (x :| xs) = case NE.nonEmpty xs of
      Nothing          -> True
      Just ys@(y :| _) -> x < y && ascending ys

prop_extract_duplicate :: Property
prop_extract_duplicate = property $ do
    n <- forAll neIntMapGen
    tripping n duplicate
               (Identity . extract)

prop_fmap_extract_duplicate :: Property
prop_fmap_extract_duplicate = property $ do
    n <- forAll neIntMapGen
    tripping n duplicate
               (Identity . fmap extract)

prop_duplicate_duplicate :: Property
prop_duplicate_duplicate = property $ do
    n <- forAll neIntMapGen
    let dd1 = duplicate . duplicate      $ n
        dd2 = fmap duplicate . duplicate $ n
    assert $ NEM.valid dd1
    assert $ NEM.valid dd2
    dd1 === dd2






prop_insertMapWithKey :: Property
prop_insertMapWithKey = ttProp (gf3 valGen :?> GTIntKey :-> GTVal :-> GTIntMap :-> TTNEIntMap)
    M.insertWithKey
    NEM.insertMapWithKey

prop_singleton :: Property
prop_singleton = ttProp (GTIntKey :-> GTVal :-> TTNEIntMap)
    M.singleton
    NEM.singleton

prop_fromSet :: Property
prop_fromSet = ttProp (gf1 valGen :?> GTNEIntSet :-> TTNEIntMap)
    M.fromSet
    NEM.fromSet

prop_fromAscList :: Property
prop_fromAscList = ttProp (GTSorted STAsc (GTNEList Nothing (GTIntKey :&: GTVal)) :-> TTNEIntMap)
    M.fromAscList
    NEM.fromAscList

prop_fromAscListWithKey :: Property
prop_fromAscListWithKey = ttProp (gf3 valGen :?> GTSorted STAsc (GTNEList Nothing (GTIntKey :&: GTVal)) :-> TTNEIntMap)
    M.fromAscListWithKey
    NEM.fromAscListWithKey

prop_fromDistinctAscList :: Property
prop_fromDistinctAscList = ttProp (GTSorted STDistinctAsc (GTNEList Nothing (GTIntKey :&: GTVal)) :-> TTNEIntMap)
    M.fromDistinctAscList
    NEM.fromDistinctAscList

prop_fromListWithKey :: Property
prop_fromListWithKey = ttProp (gf3 valGen :?> GTNEList Nothing (GTIntKey :&: GTVal) :-> TTNEIntMap)
    M.fromListWithKey
    NEM.fromListWithKey

prop_insert :: Property
prop_insert = ttProp (GTIntKey :-> GTVal :-> GTNEIntMap :-> TTNEIntMap)
    M.insert
    NEM.insert

prop_insertWithKey :: Property
prop_insertWithKey = ttProp (gf3 valGen :?> GTIntKey :-> GTVal :-> GTNEIntMap :-> TTNEIntMap)
    M.insertWithKey
    NEM.insertWithKey

prop_delete :: Property
prop_delete = ttProp (GTIntKey :-> GTNEIntMap :-> TTOther)
    M.delete
    NEM.delete

prop_adjustWithKey :: Property
prop_adjustWithKey = ttProp (gf2 valGen :?> GTIntKey :-> GTNEIntMap :-> TTNEIntMap)
    M.adjustWithKey
    NEM.adjustWithKey

prop_updateWithKey :: Property
prop_updateWithKey = ttProp (gf2 (Gen.maybe valGen) :?> GTIntKey :-> GTNEIntMap :-> TTOther)
    M.updateWithKey
    NEM.updateWithKey

prop_updateLookupWithKey :: Property
prop_updateLookupWithKey = ttProp (gf2 (Gen.maybe valGen) :?> GTIntKey :-> GTNEIntMap :-> TTMaybe TTVal :*: TTOther)
    M.updateLookupWithKey
    NEM.updateLookupWithKey

prop_alter :: Property
prop_alter = ttProp (gf1 (Gen.maybe valGen) :?> GTIntKey :-> GTNEIntMap :-> TTOther)
    M.alter
    NEM.alter

prop_alter' :: Property
prop_alter' = ttProp (gf1 valGen :?> GTIntKey :-> GTNEIntMap :-> TTNEIntMap)
    (M.alter . fmap Just)
    NEM.alter'

prop_alterF :: Property
prop_alterF = ttProp ( gf1 (Gen.maybe valGen)
                   :?> GTIntKey
                   :-> GTNEIntMap
                   :-> TTCtx (GTMaybe GTVal :-> TTOther) (TTMaybe TTVal)
                     )
    (M.alterF   . Context)
    (NEM.alterF . Context)

prop_alterF_rules_Const :: Property
prop_alterF_rules_Const = ttProp ( gf1 (Const <$> valGen)
                               :?> GTIntKey
                               :-> GTNEIntMap
                               :-> TTOther
                                 )
    (\f k m -> getConst (M.alterF   f k m))
    (\f k m -> getConst (NEM.alterF f k m))

prop_alterF_rules_Identity :: Property
prop_alterF_rules_Identity = ttProp ( gf1 (Identity <$> Gen.maybe valGen)
                                  :?> GTIntKey
                                  :-> GTNEIntMap
                                  :-> TTOther
                                    )
    (\f k m -> runIdentity (M.alterF   f k m))
    (\f k m -> runIdentity (NEM.alterF f k m))

prop_alterF' :: Property
prop_alterF' = ttProp (gf1 valGen :?> GTIntKey :-> GTNEIntMap :-> TTCtx (GTVal :-> TTNEIntMap) (TTMaybe TTVal))
    (M.alterF    . Context . fmap Just)
    (NEM.alterF' . Context)

prop_alterF'_rules_Const :: Property
prop_alterF'_rules_Const = ttProp ( gf1 (Const <$> valGen)
                                :?> GTIntKey
                                :-> GTNEIntMap
                                :-> TTOther
                                  )
    (\f k m -> let f' = fmap Just . f in getConst (M.alterF    f' k m))
    (\f k m -> getConst (NEM.alterF' f k m))

-- -- | This fails, but isn't possible to fix without copying-and-pasting more
-- -- in code from containers.
-- prop_alterF'_rules_Identity :: Property
-- prop_alterF'_rules_Identity = ttProp ( gf1 (Identity <$> valGen)
--                                    :?> GTIntKey
--                                    :-> GTNEIntMap
--                                    :-> TTNEIntMap
--                                      )
--     (\f k m -> let f' = fmap Just . f in runIdentity (M.alterF   f' k m))
--     (\f k m -> runIdentity (NEM.alterF' f k m))

prop_lookup :: Property
prop_lookup = ttProp (GTIntKey :-> GTNEIntMap :-> TTMaybe TTVal)
    M.lookup
    NEM.lookup

prop_findWithDefault :: Property
prop_findWithDefault = ttProp (GTVal :-> GTIntKey :-> GTNEIntMap :-> TTVal)
    M.findWithDefault
    NEM.findWithDefault

prop_member :: Property
prop_member = ttProp (GTIntKey :-> GTNEIntMap :-> TTOther)
    M.member
    NEM.member

prop_notMember :: Property
prop_notMember = ttProp (GTIntKey :-> GTNEIntMap :-> TTOther)
    M.notMember
    NEM.notMember

prop_lookupLT :: Property
prop_lookupLT = ttProp (GTIntKey :-> GTNEIntMap :-> TTMaybe (TTOther :*: TTVal))
    M.lookupLT
    NEM.lookupLT

prop_lookupGT :: Property
prop_lookupGT = ttProp (GTIntKey :-> GTNEIntMap :-> TTMaybe (TTOther :*: TTVal))
    M.lookupGT
    NEM.lookupGT

prop_lookupLE :: Property
prop_lookupLE = ttProp (GTIntKey :-> GTNEIntMap :-> TTMaybe (TTOther :*: TTVal))
    M.lookupLE
    NEM.lookupLE

prop_lookupGE :: Property
prop_lookupGE = ttProp (GTIntKey :-> GTNEIntMap :-> TTMaybe (TTOther :*: TTVal))
    M.lookupGE
    NEM.lookupGE

prop_size :: Property
prop_size = ttProp (GTNEIntMap :-> TTOther)
    M.size
    NEM.size

prop_union :: Property
prop_union = ttProp (GTNEIntMap :-> GTNEIntMap :-> TTNEIntMap)
    M.union
    NEM.union

prop_unionWith :: Property
prop_unionWith = ttProp (gf2 valGen :?> GTNEIntMap :-> GTNEIntMap :-> TTNEIntMap)
    M.unionWith
    NEM.unionWith

prop_unionWithKey :: Property
prop_unionWithKey = ttProp (gf3 valGen :?> GTNEIntMap :-> GTNEIntMap :-> TTNEIntMap)
    M.unionWithKey
    NEM.unionWithKey

prop_unions :: Property
prop_unions = ttProp (GTNEList (Just (Range.linear 2 5)) GTNEIntMap :-> TTNEIntMap)
    M.unions
    NEM.unions

prop_unionsWith :: Property
prop_unionsWith = ttProp (gf2 valGen :?> GTNEList (Just (Range.linear 2 5)) GTNEIntMap :-> TTNEIntMap)
    M.unionsWith
    NEM.unionsWith

prop_difference :: Property
prop_difference = ttProp (GTNEIntMap :-> GTNEIntMap :-> TTOther)
    M.difference
    NEM.difference

prop_differenceWithKey :: Property
prop_differenceWithKey = ttProp (gf3 (Gen.maybe valGen) :?> GTNEIntMap :-> GTNEIntMap :-> TTOther)
    M.differenceWithKey
    NEM.differenceWithKey

prop_intersection :: Property
prop_intersection = ttProp (GTNEIntMap :-> GTNEIntMap :-> TTOther)
    M.intersection
    NEM.intersection

prop_intersectionWithKey :: Property
prop_intersectionWithKey = ttProp (gf3 valGen :?> GTNEIntMap :-> GTNEIntMap :-> TTOther)
    M.intersectionWithKey
    NEM.intersectionWithKey

prop_map :: Property
prop_map = ttProp (gf1 valGen :?> GTNEIntMap :-> TTNEIntMap)
    M.map
    NEM.map

prop_map_rules_map :: Property
prop_map_rules_map = ttProp (gf1 valGen :?> gf1 valGen :?> GTNEIntMap :-> TTNEIntMap)
    (\f g xs -> M.map   f (M.map   g xs))
    (\f g xs -> NEM.map f (NEM.map g xs))

prop_map_rules_coerce :: Property
prop_map_rules_coerce = ttProp (GTNEIntMap :-> TTNEIntMap)
    (M.map   @Text @Text coerce)
    (NEM.map @Text @Text coerce)

prop_map_rules_mapWithKey :: Property
prop_map_rules_mapWithKey = ttProp (gf1 valGen :?> gf2 valGen :?> GTNEIntMap :-> TTNEIntMap)
    (\f g xs -> M.map f (M.mapWithKey   g xs))
    (\f g xs -> NEM.map f (NEM.mapWithKey g xs))

prop_mapWithKey :: Property
prop_mapWithKey = ttProp (gf2 valGen :?> GTNEIntMap :-> TTNEIntMap)
    M.mapWithKey
    NEM.mapWithKey

prop_mapWithKey_rules_mapWithKey :: Property
prop_mapWithKey_rules_mapWithKey = ttProp (gf2 valGen :?> gf2 valGen :?> GTNEIntMap :-> TTNEIntMap)
    (\f g xs -> M.mapWithKey   f (M.mapWithKey   g xs))
    (\f g xs -> NEM.mapWithKey f (NEM.mapWithKey g xs))

prop_mapWithKey_rules_map :: Property
prop_mapWithKey_rules_map = ttProp (gf2 valGen :?> gf1 valGen :?> GTNEIntMap :-> TTNEIntMap)
    (\f g xs -> M.mapWithKey   f (M.map   g xs))
    (\f g xs -> NEM.mapWithKey f (NEM.map g xs))

-- | These intentionally do not match, because Foldable for IntMap is
-- inconsistent
-- prop_traverseWithKey1 :: Property
-- prop_traverseWithKey1 = ttProp (gf1 valGen :?> GTNEIntMap :-> TTBazaar GTVal TTNEIntMap TTVal)
--     (\f -> M.traverseWithKey    (\k -> (`More` Done (f . (k,)))))
--     (\f -> NEM.traverseWithKey1 (\k -> (`More` Done (f . (k,)))))

-- prop_traverseWithKey :: Property
-- prop_traverseWithKey = ttProp (gf1 valGen :?> GTNEIntMap :-> TTBazaar GTVal TTNEIntMap TTVal)
--     (\f -> M.traverseWithKey   (\k -> (`More` Done (f . (k,)))))
--     (\f -> NEM.traverseWithKey (\k -> (`More` Done (f . (k,)))))

-- prop_sequence1 :: Property
-- prop_sequence1 = ttProp (GTNEIntMap :-> TTBazaar GTVal TTNEIntMap TTVal)
--     (sequenceA . fmap (`More` Done id))
--     (sequence1 . fmap (`More` Done id))

-- prop_sequenceA :: Property
-- prop_sequenceA = ttProp (GTNEIntMap :-> TTBazaar GTVal TTNEIntMap TTVal)
--     (sequenceA . fmap (`More` Done id))
--     (sequenceA . fmap (`More` Done id))

-- prop_mapAccumWithKey :: Property
-- prop_mapAccumWithKey = ttProp  ( gf3 ((,) <$> valGen <*> valGen)
--                              :?> GTOther valGen
--                              :-> GTNEIntMap
--                              :-> TTOther :*: TTNEIntMap
--                                )
--     M.mapAccumWithKey
--     NEM.mapAccumWithKey

-- prop_mapAccumRWithKey :: Property
-- prop_mapAccumRWithKey = ttProp  ( gf3 ((,) <$> valGen <*> valGen)
--                               :?> GTOther valGen
--                               :-> GTNEIntMap
--                               :-> TTOther :*: TTNEIntMap
--                                 )
--     M.mapAccumRWithKey
--     NEM.mapAccumRWithKey

prop_mapKeys :: Property
prop_mapKeys = ttProp (gf1 intKeyGen :?> GTNEIntMap :-> TTNEIntMap)
    M.mapKeys
    NEM.mapKeys

prop_mapKeysWith :: Property
prop_mapKeysWith = ttProp ( gf2 valGen
                        :?> gf1 intKeyGen
                        :?> GTNEIntMap
                        :-> TTNEIntMap
                          )
    M.mapKeysWith
    NEM.mapKeysWith

prop_mapKeysMonotonic :: Property
prop_mapKeysMonotonic = ttProp (GTNEIntMap :-> TTNEIntMap)
    (M.mapKeysMonotonic   (*2))
    (NEM.mapKeysMonotonic (*2))

prop_foldr :: Property
prop_foldr = ttProp ( gf2 valGen
                  :?> GTOther valGen
                  :-> GTNEIntMap
                  :-> TTOther
                    )
    M.foldr
    NEM.foldr

prop_foldl :: Property
prop_foldl = ttProp ( gf2 valGen
                  :?> GTOther valGen
                  :-> GTNEIntMap
                  :-> TTOther
                    )
    M.foldl
    NEM.foldl

prop_foldr1 :: Property
prop_foldr1 = ttProp ( gf2 valGen
                   :?> GTNEIntMap
                   :-> TTOther
                     )
    foldr1
    NEM.foldr1

prop_foldl1 :: Property
prop_foldl1 = ttProp ( gf2 valGen
                   :?> GTNEIntMap
                   :-> TTOther
                     )
    foldl1
    NEM.foldl1

prop_foldrWithKey :: Property
prop_foldrWithKey = ttProp ( gf3 valGen
                         :?> GTOther valGen
                         :-> GTNEIntMap
                         :-> TTOther
                           )
    M.foldrWithKey
    NEM.foldrWithKey

prop_foldlWithKey :: Property
prop_foldlWithKey = ttProp ( gf3 valGen
                         :?> GTOther valGen
                         :-> GTNEIntMap
                         :-> TTOther
                           )
    M.foldlWithKey
    NEM.foldlWithKey

prop_foldMapWithKey :: Property
prop_foldMapWithKey = ttProp (gf2 valGen :?> GTNEIntMap :-> TTOther)
    (\f -> foldMap (uncurry f) . M.toList)
    NEM.foldMapWithKey

prop_foldr' :: Property
prop_foldr' = ttProp ( gf2 valGen
                   :?> GTOther valGen
                   :-> GTNEIntMap
                   :-> TTOther
                     )
    M.foldr'
    NEM.foldr'

prop_foldl' :: Property
prop_foldl' = ttProp ( gf2 valGen
                   :?> GTOther valGen
                   :-> GTNEIntMap
                   :-> TTOther
                     )
    M.foldl'
    NEM.foldl'

prop_foldr1' :: Property
prop_foldr1' = ttProp ( gf2 valGen
                    :?> GTNEIntMap
                    :-> TTOther
                      )
    foldr1
    NEM.foldr1'

prop_foldl1' :: Property
prop_foldl1' = ttProp ( gf2 valGen
                    :?> GTNEIntMap
                    :-> TTOther
                      )
    foldl1
    NEM.foldl1'

prop_foldrWithKey' :: Property
prop_foldrWithKey' = ttProp ( gf3 valGen
                          :?> GTOther valGen
                          :-> GTNEIntMap
                          :-> TTOther
                            )
    M.foldrWithKey'
    NEM.foldrWithKey'

prop_foldlWithKey' :: Property
prop_foldlWithKey' = ttProp ( gf3 valGen
                          :?> GTOther valGen
                          :-> GTNEIntMap
                          :-> TTOther
                            )
    M.foldlWithKey'
    NEM.foldlWithKey'

prop_elems :: Property
prop_elems = ttProp (GTNEIntMap :-> TTNEList TTVal)
    M.elems
    NEM.elems

prop_keys :: Property
prop_keys = ttProp (GTNEIntMap :-> TTNEList TTOther)
    M.keys
    NEM.keys

prop_assocs :: Property
prop_assocs = ttProp (GTNEIntMap :-> TTNEList (TTOther :*: TTVal))
    M.assocs
    NEM.assocs

prop_keysSet :: Property
prop_keysSet = ttProp (GTNEIntMap :-> TTNEIntSet)
    M.keysSet
    NEM.keysSet

prop_toList :: Property
prop_toList = ttProp (GTNEIntMap :-> TTNEList (TTOther :*: TTVal))
    M.toList
    NEM.toList

prop_toDescList :: Property
prop_toDescList = ttProp (GTNEIntMap :-> TTNEList (TTOther :*: TTVal))
    M.toDescList
    NEM.toDescList

prop_filter :: Property
prop_filter = ttProp (gf1 Gen.bool :?> GTNEIntMap :-> TTOther)
    M.filter
    NEM.filter

prop_filterWithKey :: Property
prop_filterWithKey = ttProp (gf2 Gen.bool :?> GTNEIntMap :-> TTOther)
    M.filterWithKey
    NEM.filterWithKey

prop_restrictKeys :: Property
prop_restrictKeys = ttProp (GTNEIntMap :-> GTIntSet :-> TTOther)
    M.restrictKeys
    NEM.restrictKeys

prop_withoutKeys :: Property
prop_withoutKeys = ttProp (GTNEIntMap :-> GTIntSet :-> TTOther)
    M.withoutKeys
    NEM.withoutKeys

prop_partitionWithKey :: Property
prop_partitionWithKey = ttProp (gf2 Gen.bool :?> GTNEIntMap :-> TTThese TTNEIntMap TTNEIntMap)
    M.partitionWithKey
    NEM.partitionWithKey

prop_mapMaybeWithKey :: Property
prop_mapMaybeWithKey = ttProp (gf2 (Gen.maybe valGen) :?> GTNEIntMap :-> TTOther)
    M.mapMaybeWithKey
    NEM.mapMaybeWithKey

prop_mapEitherWithKey :: Property
prop_mapEitherWithKey = ttProp ( gf2 (Gen.choice [Left <$> valGen, Right <$> valGen])
                             :?> GTNEIntMap
                             :-> TTThese TTNEIntMap TTNEIntMap
                               )
    M.mapEitherWithKey
    NEM.mapEitherWithKey

prop_split :: Property
prop_split = ttProp (GTIntKey :-> GTNEIntMap :-> TTMThese TTNEIntMap TTNEIntMap)
    M.split
    NEM.split

prop_splitLookup :: Property
prop_splitLookup = ttProp (GTIntKey :-> GTNEIntMap :-> TTTThese TTVal TTNEIntMap TTNEIntMap)
    (\k -> (\(x,y,z) -> (y,x,z)) . M.splitLookup k)
    NEM.splitLookup

prop_isSubmapOfBy :: Property
prop_isSubmapOfBy = ttProp (gf2 Gen.bool :?> GTNEIntMap :-> GTNEIntMap :-> TTOther)
    M.isSubmapOfBy
    NEM.isSubmapOfBy

prop_isProperSubmapOfBy :: Property
prop_isProperSubmapOfBy = ttProp (gf2 Gen.bool :?> GTNEIntMap :-> GTNEIntMap :-> TTOther)
    M.isProperSubmapOfBy
    NEM.isProperSubmapOfBy

prop_findMin :: Property
prop_findMin = ttProp (GTNEIntMap :-> TTOther :*: TTVal)
    M.findMin
    NEM.findMin

prop_findMax :: Property
prop_findMax = ttProp (GTNEIntMap :-> TTOther :*: TTVal)
    M.findMax
    NEM.findMax

prop_deleteMin :: Property
prop_deleteMin = ttProp (GTNEIntMap :-> TTOther)
    M.deleteMin
    NEM.deleteMin

prop_deleteMax :: Property
prop_deleteMax = ttProp (GTNEIntMap :-> TTOther)
    M.deleteMax
    NEM.deleteMax

prop_deleteFindMin :: Property
prop_deleteFindMin = ttProp (GTNEIntMap :-> (TTOther :*: TTVal) :*: TTOther)
    M.deleteFindMin
    NEM.deleteFindMin

prop_deleteFindMax :: Property
prop_deleteFindMax = ttProp (GTNEIntMap :-> (TTOther :*: TTVal) :*: TTOther)
    M.deleteFindMax
    NEM.deleteFindMax

prop_updateMinWithKey :: Property
prop_updateMinWithKey = ttProp (gf2 (Gen.maybe valGen) :?> GTNEIntMap :-> TTOther)
    M.updateMinWithKey
    NEM.updateMinWithKey

prop_updateMaxWithKey :: Property
prop_updateMaxWithKey = ttProp (gf2 (Gen.maybe valGen) :?> GTNEIntMap :-> TTOther)
    M.updateMaxWithKey
    NEM.updateMaxWithKey

prop_adjustMinWithKey :: Property
prop_adjustMinWithKey = ttProp (gf2 valGen :?> GTNEIntMap :-> TTNEIntMap)
    (M.updateMinWithKey  . (fmap . fmap) Just)
    NEM.adjustMinWithKey

prop_adjustMaxWithKey :: Property
prop_adjustMaxWithKey = ttProp (gf2 valGen :?> GTNEIntMap :-> TTNEIntMap)
    (M.updateMaxWithKey  . (fmap . fmap) Just)
    NEM.adjustMaxWithKey

prop_minView :: Property
prop_minView = ttProp (GTNEIntMap :-> TTMaybe (TTVal :*: TTOther))
    M.minView
    (Just . NEM.minView)

prop_maxView :: Property
prop_maxView = ttProp (GTNEIntMap :-> TTMaybe (TTVal :*: TTOther))
    M.maxView
    (Just . NEM.maxView)

prop_elem :: Property
prop_elem = ttProp (GTVal :-> GTNEIntMap :-> TTOther)
    elem
    elem

prop_fold1 :: Property
prop_fold1 = ttProp (GTNEIntMap :-> TTVal)
    (fold . toList)
    fold1

prop_fold :: Property
prop_fold = ttProp (GTNEIntMap :-> TTVal)
    (fold . toList)
    fold

prop_foldMap1 :: Property
prop_foldMap1 = ttProp (gf1 valGen :?> GTNEIntMap :-> TTOther)
    (\f -> foldMap  ((:[]) . f) . toList)
    (\f -> foldMap1 ((:[]) . f))

prop_foldMap :: Property
prop_foldMap = ttProp (gf1 valGen :?> GTNEIntMap :-> TTOther)
    (\f -> foldMap ((:[]) . f) . toList)
    (\f -> foldMap ((:[]) . f))

prop_alt :: Property
prop_alt = ttProp (GTNEIntMap :-> GTNEIntMap :-> TTNEIntMap)
    (<!>)
    (<!>)
