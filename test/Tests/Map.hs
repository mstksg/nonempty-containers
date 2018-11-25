{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Tests.Map (mapTests) where

import           Data.Foldable
import           Data.Function
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import           Hedgehog
import           Tests.Map.Util
import qualified Data.Map                   as M
import qualified Data.Map.NonEmpty          as NEM
import qualified Data.Map.NonEmpty.Internal as NEM
import qualified Data.Text                  as T
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

mapTests :: Group
mapTests = $$(discover)


dummyKey :: KeyType
dummyKey = K 0 "hello"

combiner :: KeyType -> T.Text -> T.Text -> T.Text
combiner (K n _) v u
    | even (T.length v) = T.reverse v <> u
    | even n            = v <> u
    | otherwise         = u <> v
  
adjuster :: KeyType -> T.Text -> T.Text
adjuster (K i _)
    | even i    = T.reverse
    | otherwise = T.intersperse '_'

updater :: KeyType -> T.Text -> Maybe T.Text
updater (K i _)
    | even i    = Just . T.reverse
    | otherwise = const Nothing

mapper :: T.Text -> T.Text
mapper t
    | even (T.length t) = T.reverse t
    | otherwise         = T.intersperse '_' t





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
prop_insertMapWithKey = ttProp (GTKey :-> GTVal :-> GTNEMap :-> TTNEMap)
    (M.insertWithKey   combiner)
    (NEM.insertWithKey combiner)

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
prop_fromAscListWithKey = ttProp (GTSorted STAsc (GTNEList Nothing (GTKey :&: GTVal)) :-> TTNEMap)
    (M.fromAscListWithKey   combiner)
    (NEM.fromAscListWithKey combiner)

prop_fromDescListWithKey :: Property
prop_fromDescListWithKey = ttProp (GTSorted STDesc (GTNEList Nothing (GTKey :&: GTVal)) :-> TTNEMap)
    (M.fromDescListWithKey   combiner)
    (NEM.fromDescListWithKey combiner)

prop_fromDistinctAscList :: Property
prop_fromDistinctAscList = ttProp (GTSorted STDistinctAsc (GTNEList Nothing (GTKey :&: GTVal)) :-> TTNEMap)
    M.fromDistinctAscList
    NEM.fromDistinctAscList

prop_fromDistinctDescList :: Property
prop_fromDistinctDescList = ttProp (GTSorted STDistinctDesc (GTNEList Nothing (GTKey :&: GTVal)) :-> TTNEMap)
    M.fromDistinctDescList
    NEM.fromDistinctDescList

prop_fromListWithKey :: Property
prop_fromListWithKey = ttProp (GTNEList Nothing (GTKey :&: GTVal) :-> TTNEMap)
    (M.fromListWithKey   combiner)
    (NEM.fromListWithKey combiner)

prop_insert :: Property
prop_insert = ttProp (GTKey :-> GTVal :-> GTNEMap :-> TTNEMap)
    M.insert
    NEM.insert

prop_insertWithKey :: Property
prop_insertWithKey = ttProp (GTKey :-> GTVal :-> GTNEMap :-> TTNEMap)
    (M.insertWithKey   combiner)
    (NEM.insertWithKey combiner)

prop_delete :: Property
prop_delete = ttProp (GTKey :-> GTNEMap :-> TTMap)
    M.delete
    NEM.delete

prop_adjustWithKey :: Property
prop_adjustWithKey = ttProp (GTKey :-> GTNEMap :-> TTNEMap)
    (M.adjustWithKey   adjuster)
    (NEM.adjustWithKey adjuster)

prop_updateWithKey :: Property
prop_updateWithKey = ttProp (GTKey :-> GTNEMap :-> TTMap)
    (M.updateWithKey   updater)
    (NEM.updateWithKey updater)

prop_updateLookupWithKey :: Property
prop_updateLookupWithKey = ttProp (GTKey :-> GTNEMap :-> TTMaybe TTVal :*: TTMap)
    (M.updateLookupWithKey   updater)
    (NEM.updateLookupWithKey updater)

prop_alter :: Property
prop_alter = ttProp (GTVal :-> GTKey :-> GTNEMap :-> TTMap)
     (\v -> M.alter   (f . fromMaybe v))
     (\v -> NEM.alter (f . fromMaybe v))
  where
    f t | even (T.length t) = Just $ T.reverse t
        | otherwise         = Nothing

prop_alter' :: Property
prop_alter' = ttProp (GTVal :-> GTKey :-> GTNEMap :-> TTNEMap)
    (\v -> M.alter    (Just . mapper . fromMaybe v))
    (\v -> NEM.alter' (       mapper . fromMaybe v))

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
prop_unionWith = ttProp (GTNEMap :-> GTNEMap :-> TTNEMap)
    (M.unionWith (combiner dummyKey))
    (NEM.unionWith (combiner dummyKey))

prop_unionWithKey :: Property
prop_unionWithKey = ttProp (GTNEMap :-> GTNEMap :-> TTNEMap)
    (M.unionWithKey combiner)
    (NEM.unionWithKey combiner)

prop_unions :: Property
prop_unions = ttProp (GTNEList (Just (Range.linear 2 5)) GTNEMap :-> TTNEMap)
    M.unions
    NEM.unions

prop_unionsWith :: Property
prop_unionsWith = ttProp (GTNEList (Just (Range.linear 2 5)) GTNEMap :-> TTNEMap)
    (M.unionsWith (combiner dummyKey))
    (NEM.unionsWith (combiner dummyKey))

prop_difference :: Property
prop_difference = ttProp (GTNEMap :-> GTNEMap :-> TTMap)
    M.difference
    NEM.difference

prop_differenceWithKey :: Property
prop_differenceWithKey = ttProp (GTNEMap :-> GTNEMap :-> TTMap)
    (M.differenceWithKey f)
    (NEM.differenceWithKey f)
  where
    f (K n _) v u
      | even n    = Just (v <> u)
      | otherwise = Nothing

prop_intersection :: Property
prop_intersection = ttProp (GTNEMap :-> GTNEMap :-> TTMap)
    M.intersection
    NEM.intersection

prop_intersectionWithKey :: Property
prop_intersectionWithKey = ttProp (GTNEMap :-> GTNEMap :-> TTMap)
    (M.intersectionWithKey combiner)
    (NEM.intersectionWithKey combiner)

prop_map :: Property
prop_map = ttProp (GTNEMap :-> TTNEMap)
    (M.map   mapper)
    (NEM.map mapper)

prop_mapWithKey :: Property
prop_mapWithKey = ttProp (GTNEMap :-> TTNEMap)
    (M.mapWithKey   adjuster)
    (NEM.mapWithKey adjuster)

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
prop_mapAccumWithKey = ttProp  ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                             :-> GTNEMap
                             :-> TTOther :*: TTNEMap
                               )
    (M.mapAccumWithKey   f)
    (NEM.mapAccumWithKey f)
  where
    f d (K i _) t
      | even i    = (sin d, t <> T.pack (show t))
      | otherwise = (d + 2, T.reverse t         )

prop_mapAccumRWithKey :: Property
prop_mapAccumRWithKey = ttProp  ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                              :-> GTNEMap
                              :-> TTOther :*: TTNEMap
                                )
    (M.mapAccumRWithKey   f)
    (NEM.mapAccumRWithKey f)
  where
    f d (K i _) t
      | even i    = (sin d, t <> T.pack (show t))
      | otherwise = (d + 2, T.reverse t         )

prop_mapKeys :: Property
prop_mapKeys = ttProp (GTNEMap :-> TTNEMap)
    (M.mapKeys   f)
    (NEM.mapKeys f)
  where
    f = overKX (`mod` 25)
  
prop_mapKeysWith :: Property
prop_mapKeysWith = ttProp (GTNEMap :-> TTNEMap)
    (M.mapKeysWith   (combiner dummyKey) f)
    (NEM.mapKeysWith (combiner dummyKey) f)
  where
    f = overKX (`mod` 25)
  
prop_mapMonotonic :: Property
prop_mapMonotonic = ttProp (GTNEMap :-> TTNEMap)
    (M.mapKeysMonotonic   (* 2))
    (NEM.mapKeysMonotonic (* 2))

prop_foldr :: Property
prop_foldr = ttProp ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                  :-> GTNEMap
                  :-> TTOther
                    )
    (M.foldr   f)
    (NEM.foldr f)
  where
    f t d
      | even (T.length t) = sin d
      | otherwise         = d + 2
  
prop_foldl :: Property
prop_foldl = ttProp ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                  :-> GTNEMap
                  :-> TTOther
                    )
    (M.foldl   f)
    (NEM.foldl f)
  where
    f d t
      | even (T.length t) = sin d
      | otherwise         = d + 2

prop_foldr1 :: Property
prop_foldr1 = ttProp ( GTNEMap
                   :-> TTOther
                     )
    (foldr1     (combiner dummyKey))
    (NEM.foldr1 (combiner dummyKey))
  
prop_foldl1 :: Property
prop_foldl1 = ttProp ( GTNEMap
                   :-> TTOther
                     )
    (foldl1     (combiner dummyKey))
    (NEM.foldl1 (combiner dummyKey))
  
prop_foldrWithKey :: Property
prop_foldrWithKey = ttProp ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                         :-> GTNEMap
                         :-> TTOther
                           )
    (M.foldrWithKey   f)
    (NEM.foldrWithKey f)
  where
    f (K i _) t d
      | even i            = cos d
      | even (T.length t) = sin d
      | otherwise         = d + 2
  
prop_foldlWithKey :: Property
prop_foldlWithKey = ttProp ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                         :-> GTNEMap
                         :-> TTOther
                           )
    (M.foldlWithKey   f)
    (NEM.foldlWithKey f)
  where
    f d (K i _) t
      | even i            = cos d
      | even (T.length t) = sin d
      | otherwise         = d + 2
  
prop_foldMapWithKey :: Property
prop_foldMapWithKey = ttProp (GTNEMap :-> TTOther)
    (M.foldMapWithKey   adjuster)
    (NEM.foldMapWithKey adjuster)
  
prop_foldr' :: Property
prop_foldr' = ttProp ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                   :-> GTNEMap
                   :-> TTOther
                     )
    (M.foldr'   f)
    (NEM.foldr' f)
  where
    f t d
      | even (T.length t) = sin d
      | otherwise         = d + 2
  
prop_foldl' :: Property
prop_foldl' = ttProp ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                   :-> GTNEMap
                   :-> TTOther
                     )
    (M.foldl'   f)
    (NEM.foldl' f)
  where
    f d t
      | even (T.length t) = sin d
      | otherwise         = d + 2

prop_foldr1' :: Property
prop_foldr1' = ttProp ( GTNEMap
                    :-> TTOther
                      )
    (foldr1      (combiner dummyKey))
    (NEM.foldr1' (combiner dummyKey))
  
prop_foldl1' :: Property
prop_foldl1' = ttProp ( GTNEMap
                    :-> TTOther
                      )
    (foldl1      (combiner dummyKey))
    (NEM.foldl1' (combiner dummyKey))
  
prop_foldrWithKey' :: Property
prop_foldrWithKey' = ttProp ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                          :-> GTNEMap
                          :-> TTOther
                            )
    (M.foldrWithKey'   f)
    (NEM.foldrWithKey' f)
  where
    f (K i _) t d
      | even i            = cos d
      | even (T.length t) = sin d
      | otherwise         = d + 2
  
prop_foldlWithKey' :: Property
prop_foldlWithKey' = ttProp ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                          :-> GTNEMap
                          :-> TTOther
                            )
    (M.foldlWithKey'   f)
    (NEM.foldlWithKey' f)
  where
    f d (K i _) t
      | even i            = cos d
      | even (T.length t) = sin d
      | otherwise         = d + 2

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
prop_filter = ttProp (GTNEMap :-> TTMap)
    (M.filter   f)
    (NEM.filter f)
  where
    f = even . T.length

prop_filterWithKey :: Property
prop_filterWithKey = ttProp (GTNEMap :-> TTMap)
    (M.filterWithKey   f)
    (NEM.filterWithKey f)
  where
    f (K i _) = even . (+ fromIntegral i) . T.length

prop_restrictKeys :: Property
prop_restrictKeys = ttProp (GTNEMap :-> GTSet GTKey :-> TTMap)
    M.restrictKeys
    NEM.restrictKeys

prop_withoutKeys :: Property
prop_withoutKeys = ttProp (GTNEMap :-> GTSet GTKey :-> TTMap)
    M.withoutKeys
    NEM.withoutKeys

prop_partitionWithKey :: Property
prop_partitionWithKey = ttProp (GTNEMap :-> TTThese TTNEMap TTNEMap)
    (M.partitionWithKey   f)
    (NEM.partitionWithKey f)
  where
    f (K i _) = even . (+ fromIntegral i) . T.length
    
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
prop_mapMaybeWithKey = ttProp (GTNEMap :-> TTMap)
    (M.mapMaybeWithKey   f)
    (NEM.mapMaybeWithKey f)
  where
    f (K i _) | even i    = Just . T.length
              | otherwise = const Nothing

prop_mapEitherWithKey :: Property
prop_mapEitherWithKey = ttProp (GTNEMap :-> TTThese TTNEMap TTNEMap)
    (M.mapEitherWithKey   f)
    (NEM.mapEitherWithKey f)
  where
    f (K i _) | even i    = Right . T.reverse
              | otherwise = Left  . T.length

prop_split :: Property
prop_split = ttProp (GTKey :-> GTNEMap :-> TTMThese TTNEMap TTNEMap)
    M.split
    NEM.split

prop_splitLookup :: Property
prop_splitLookup = ttProp (GTKey :-> GTNEMap :-> TTMaybe TTVal :*: TTMThese TTNEMap TTNEMap)
    (\k -> (\(x,y,z) -> (y,(x,z))) . M.splitLookup k)
    NEM.splitLookup

prop_isSubmapOfBy :: Property
prop_isSubmapOfBy = ttProp (GTNEMap :-> GTNEMap :-> TTOther)
    (M.isSubmapOfBy f)
    (NEM.isSubmapOfBy f)
  where
    f = (<) `on` T.length

prop_isProperSubmapOfBy :: Property
prop_isProperSubmapOfBy = ttProp (GTNEMap :-> GTNEMap :-> TTOther)
    (M.isProperSubmapOfBy f)
    (NEM.isProperSubmapOfBy f)
  where
    f = (<) `on` T.length

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
prop_updateMinWithKey = ttProp (GTNEMap :-> TTMap)
    (M.updateMinWithKey   updater)
    (NEM.updateMinWithKey updater)

prop_updateMaxWithKey :: Property
prop_updateMaxWithKey = ttProp (GTNEMap :-> TTMap)
    (M.updateMaxWithKey   updater)
    (NEM.updateMaxWithKey updater)

prop_adjustMinWithKey :: Property
prop_adjustMinWithKey = ttProp (GTNEMap :-> TTNEMap)
    (M.updateMinWithKey   (\i -> Just . adjuster i))
    (NEM.adjustMinWithKey adjuster)

prop_adjustMaxWithKey :: Property
prop_adjustMaxWithKey = ttProp (GTNEMap :-> TTNEMap)
    (M.updateMaxWithKey   (\i -> Just . adjuster i))
    (NEM.adjustMaxWithKey adjuster)

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

