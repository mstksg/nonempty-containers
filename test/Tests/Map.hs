{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Tests.Map (mapTests) where

import           Control.Applicative
import           Control.Comonad
import           Data.Coerce
import           Data.Foldable
import           Data.Functor.Alt
import           Data.Functor.Identity
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import           Data.Text                  (Text)
import           Hedgehog
import           Test.Tasty
import           Tests.Util
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Map.NonEmpty          as NEM
import qualified Data.Map.NonEmpty.Internal as NEM
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

mapTests :: TestTree
mapTests = groupTree $$(discover)





prop_valid :: Property
prop_valid = property $
    assert . NEM.valid =<< forAll neMapGen

prop_valid_toMap :: Property
prop_valid_toMap = property $
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

prop_read_show :: Property
prop_read_show = readShow neMapGen

prop_read1_show1 :: Property
prop_read1_show1 = readShow1 neMapGen

prop_show_show1 :: Property
prop_show_show1 = showShow1 neMapGen

prop_show_show2 :: Property
prop_show_show2 = showShow2 neMapGen

prop_splitRoot :: Property
prop_splitRoot = property $ do
    n <- forAll neMapGen
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
    n <- forAll neMapGen
    tripping n duplicate
               (Identity . extract)

prop_fmap_extract_duplicate :: Property
prop_fmap_extract_duplicate = property $ do
    n <- forAll neMapGen
    tripping n duplicate
               (Identity . fmap extract)

prop_duplicate_duplicate :: Property
prop_duplicate_duplicate = property $ do
    n <- forAll neMapGen
    let dd1 = duplicate . duplicate      $ n
        dd2 = fmap duplicate . duplicate $ n
    assert $ NEM.valid dd1
    assert $ NEM.valid dd2
    dd1 === dd2








prop_insertMapWithKey :: Property
prop_insertMapWithKey = ttProp (gf3 valGen :?> GTKey :-> GTVal :-> GTMap :-> TTNEMap)
    M.insertWithKey
    NEM.insertMapWithKey

prop_singleton :: Property
prop_singleton = ttProp (GTKey :-> GTVal :-> TTNEMap)
    M.singleton
    NEM.singleton

prop_fromSet :: Property
prop_fromSet = ttProp (gf1 valGen :?> GTNESet :-> TTNEMap)
    M.fromSet
    NEM.fromSet

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
prop_alterF = ttProp ( gf1 (Gen.maybe valGen)
                   :?> GTKey
                   :-> GTNEMap
                   :-> TTCtx (GTMaybe GTVal :-> TTMap) (TTMaybe TTVal)
                     )
    (M.alterF   . Context)
    (NEM.alterF . Context)

prop_alterF_rules_Const :: Property
prop_alterF_rules_Const = ttProp ( gf1 (Const <$> valGen)
                               :?> GTKey
                               :-> GTNEMap
                               :-> TTOther
                                 )
    (\f k m -> getConst (M.alterF   f k m))
    (\f k m -> getConst (NEM.alterF f k m))

prop_alterF_rules_Identity :: Property
prop_alterF_rules_Identity = ttProp ( gf1 (Identity <$> Gen.maybe valGen)
                                  :?> GTKey
                                  :-> GTNEMap
                                  :-> TTMap
                                    )
    (\f k m -> runIdentity (M.alterF   f k m))
    (\f k m -> runIdentity (NEM.alterF f k m))

prop_alterF' :: Property
prop_alterF' = ttProp (gf1 valGen :?> GTKey :-> GTNEMap :-> TTCtx (GTVal :-> TTNEMap) (TTMaybe TTVal))
    (M.alterF    . Context . fmap Just)
    (NEM.alterF' . Context)

prop_alterF'_rules_Const :: Property
prop_alterF'_rules_Const = ttProp ( gf1 (Const <$> valGen)
                                :?> GTKey
                                :-> GTNEMap
                                :-> TTOther
                                  )
    (\f k m -> let f' = fmap Just . f in getConst (M.alterF    f' k m))
    (\f k m -> getConst (NEM.alterF' f k m))

-- -- | This fails, but isn't possible to fix without copying-and-pasting more
-- -- in code from containers.
-- prop_alterF'_rules_Identity :: Property
-- prop_alterF'_rules_Identity = ttProp ( gf1 (Identity <$> valGen)
--                                    :?> GTKey
--                                    :-> GTNEMap
--                                    :-> TTNEMap
--                                      )
--     (\f k m -> let f' = fmap Just . f in runIdentity (M.alterF   f' k m))
--     (\f k m -> runIdentity (NEM.alterF' f k m))

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

prop_map_rules_map :: Property
prop_map_rules_map = ttProp (gf1 valGen :?> gf1 valGen :?> GTNEMap :-> TTNEMap)
    (\f g xs -> M.map   f (M.map   g xs))
    (\f g xs -> NEM.map f (NEM.map g xs))

prop_map_rules_coerce :: Property
prop_map_rules_coerce = ttProp (GTNEMap :-> TTNEMap)
    (M.map   @Text @Text coerce)
    (NEM.map @Text @Text coerce)

prop_map_rules_mapWithKey :: Property
prop_map_rules_mapWithKey = ttProp (gf1 valGen :?> gf2 valGen :?> GTNEMap :-> TTNEMap)
    (\f g xs -> M.map f (M.mapWithKey   g xs))
    (\f g xs -> NEM.map f (NEM.mapWithKey g xs))

prop_mapWithKey :: Property
prop_mapWithKey = ttProp (gf2 valGen :?> GTNEMap :-> TTNEMap)
    M.mapWithKey
    NEM.mapWithKey

prop_mapWithKey_rules_mapWithKey :: Property
prop_mapWithKey_rules_mapWithKey = ttProp (gf2 valGen :?> gf2 valGen :?> GTNEMap :-> TTNEMap)
    (\f g xs -> M.mapWithKey   f (M.mapWithKey   g xs))
    (\f g xs -> NEM.mapWithKey f (NEM.mapWithKey g xs))

prop_mapWithKey_rules_map :: Property
prop_mapWithKey_rules_map = ttProp (gf2 valGen :?> gf1 valGen :?> GTNEMap :-> TTNEMap)
    (\f g xs -> M.mapWithKey   f (M.map   g xs))
    (\f g xs -> NEM.mapWithKey f (NEM.map g xs))

prop_traverseWithKey1 :: Property
prop_traverseWithKey1 = ttProp (gf2 valGen :?> GTNEMap :-> TTBazaar GTVal TTNEMap TTVal)
    (\f -> M.traverseWithKey    (\k -> (`More` Done (f k))))
    (\f -> NEM.traverseWithKey1 (\k -> (`More` Done (f k))))

prop_traverseWithKey :: Property
prop_traverseWithKey = ttProp (gf2 valGen :?> GTNEMap :-> TTBazaar GTVal TTNEMap TTVal)
    (\f -> M.traverseWithKey   (\k -> (`More` Done (f k))))
    (\f -> NEM.traverseWithKey (\k -> (`More` Done (f k))))

prop_traverseMaybeWithKey1 :: Property
prop_traverseMaybeWithKey1 = ttProp (gf2 valGen :?> GTNEMap :-> TTBazaar (GTMaybe GTVal) TTMap TTVal)
    (\f -> M.traverseMaybeWithKey    (\k -> (`More` Done (fmap (f k)))))
    (\f -> NEM.traverseMaybeWithKey1 (\k -> (`More` Done (fmap (f k)))))

prop_traverseMaybeWithKey :: Property
prop_traverseMaybeWithKey = ttProp (gf2 valGen :?> GTNEMap :-> TTBazaar (GTMaybe GTVal) TTMap TTVal)
    (\f -> M.traverseMaybeWithKey   (\k -> (`More` Done (fmap (f k)))))
    (\f -> NEM.traverseMaybeWithKey (\k -> (`More` Done (fmap (f k)))))

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

prop_mapKeysMonotonic :: Property
prop_mapKeysMonotonic = ttProp (GF valGen go :?> GTNEMap :-> TTNEMap)
    M.mapKeysMonotonic
    NEM.mapKeysMonotonic
  where
    go f (K i t) = K (i * 2) (f t)

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

prop_keysSet :: Property
prop_keysSet = ttProp (GTNEMap :-> TTNESet)
    M.keysSet
    NEM.keysSet

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
prop_restrictKeys = ttProp (GTNEMap :-> GTSet :-> TTMap)
    M.restrictKeys
    NEM.restrictKeys

prop_withoutKeys :: Property
prop_withoutKeys = ttProp (GTNEMap :-> GTSet :-> TTMap)
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
prop_splitLookup = ttProp (GTKey :-> GTNEMap :-> TTTThese TTVal TTNEMap TTNEMap)
    (\k -> (\(x,y,z) -> (y,x,z)) . M.splitLookup k)
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

prop_elemAt :: Property
prop_elemAt = ttProp (GTSize :-> GTNEMap :-> TTKey :*: TTVal)
    (\i m -> M.elemAt   (i `mod` M.size   m) m)
    (\i m -> NEM.elemAt (i `mod` NEM.size m) m)

prop_adjustAt :: Property
prop_adjustAt = ttProp (gf2 valGen :?> GTSize :-> GTNEMap :-> TTNEMap)
    (\f i m -> M.updateAt   (\k -> Just . f k) (i `mod` M.size   m) m)
    (\f i m -> NEM.adjustAt f                  (i `mod` NEM.size m) m)

prop_updateAt :: Property
prop_updateAt = ttProp (gf2 (Gen.maybe valGen) :?> GTSize :-> GTNEMap :-> TTMap)
    (\f i m -> M.updateAt   f (i `mod` M.size   m) m)
    (\f i m -> NEM.updateAt f (i `mod` NEM.size m) m)

prop_deleteAt :: Property
prop_deleteAt = ttProp (GTSize :-> GTNEMap :-> TTMap)
    (\i m -> M.deleteAt   (i `mod` M.size   m) m)
    (\i m -> NEM.deleteAt (i `mod` NEM.size m) m)

prop_take :: Property
prop_take = ttProp (GTSize :-> GTNEMap :-> TTMap)
    M.take
    NEM.take

prop_drop :: Property
prop_drop = ttProp (GTSize :-> GTNEMap :-> TTMap)
    M.drop
    NEM.drop

prop_splitAt :: Property
prop_splitAt = ttProp (GTSize :-> GTNEMap :-> TTThese TTNEMap TTNEMap)
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
prop_foldMap1 = ttProp (gf1 valGen :?> GTNEMap :-> TTOther)
    (\f -> foldMap  ((:[]) . f))
    (\f -> foldMap1 ((:[]) . f))

prop_foldMap :: Property
prop_foldMap = ttProp (gf1 valGen :?> GTNEMap :-> TTOther)
    (\f -> foldMap ((:[]) . f))
    (\f -> foldMap ((:[]) . f))

prop_alt :: Property
prop_alt = ttProp (GTNEMap :-> GTNEMap :-> TTNEMap)
    (<!>)
    (<!>)
