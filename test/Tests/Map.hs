{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeInType        #-}

module Tests.Map (mapTests) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Foldable
import           Data.Function
import           Data.Functor.Identity
import           Data.Kind
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Map                   (Map)
import           Data.Map.NonEmpty          (NEMap)
import           Data.Maybe
import           Data.These
import           Hedgehog
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Map.NonEmpty          as NEM
import qualified Data.Map.NonEmpty.Internal as NEM
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

data SortType :: Type -> Type where
    STAsc          :: Ord a => SortType a
    STDesc         :: Ord a => SortType a
    STDistinctAsc  :: Ord a => SortType (a, b)
    STDistinctDesc :: Ord a => SortType (a, b)

data GenType :: Type -> Type -> Type where
    GTNEMap  :: GenType (M.Map Integer T.Text) (NEM.NEMap Integer T.Text)
    GTKey    :: GenType Integer                Integer
    GTVal    :: GenType T.Text                 T.Text
    GTOther  :: Gen a
             -> GenType a                      a
    GTMaybe  :: GenType a                      b
             -> GenType (Maybe a)              (Maybe b)
    (:&:)    :: GenType a                      b
             -> GenType c                      d
             -> GenType (a, c)                 (b, d)
    GTNEList :: Maybe (Range Int)
             -> GenType a                      b
             -> GenType [a]                    (NonEmpty b)
    GTSet    :: (Ord a, Ord b)
             => GenType a                      b
             -> GenType (S.Set a)              (S.Set b)
    GTSorted :: SortType a
             -> GenType [a]                    (NonEmpty a)
             -> GenType [a]                    (NonEmpty a)

data TestType :: Type -> Type -> Type where
    TTNEMap  :: (Eq a, Show a)
             => TestType (M.Map Integer a) (NEM.NEMap Integer a)
    TTMap    :: (Eq a, Show a)
             => TestType (M.Map Integer a) (M.Map     Integer a)
    TTKey    :: TestType Integer           Integer
    TTVal    :: TestType T.Text            T.Text
    TTOther  :: (Eq a, Show a)
             => TestType a                 a
    TTThese  :: (Eq a, Show a, Monoid a, Eq c, Show c, Monoid c)
             => TestType a                 b
             -> TestType c                 d
             -> TestType (a, c)            (These b d)
    TTMThese :: (Eq a, Show a, Monoid a, Eq c, Show c, Monoid c)
             => TestType a                 b
             -> TestType c                 d
             -> TestType (a, c)            (Maybe (These b d))
    TTMaybe  :: TestType a                 b
             -> TestType (Maybe a)         (Maybe b)
    TTNEList :: TestType a                 b
             -> TestType [a]               (NonEmpty b)
    (:*:)    :: (Eq a, Eq b, Eq c, Eq d, Show a, Show b, Show c, Show d)
             => TestType a                 b
             -> TestType c                 d
             -> TestType (a, c)            (b, d)
    (:->)    :: (Show a, Show b)
             => GenType  a                 b
             -> TestType c                 d
             -> TestType (a -> c)          (b -> d)

infixr 2 :&:
infixr 1 :->
infixr 2 :*:

runSorter
    :: SortType a
    -> [a]
    -> [a]
runSorter = \case
    STAsc          -> S.toAscList  . S.fromList
    STDesc         -> S.toDescList . S.fromList
    STDistinctAsc  -> M.toAscList  . M.fromList
    STDistinctDesc -> M.toDescList . M.fromList

runGT :: GenType a b -> Gen (a, b)
runGT = \case
    GTNEMap    -> (\n -> (NEM.IsNonEmpty n, n)) <$> neMapGen
    GTKey      -> join (,) <$> keyGen
    GTVal      -> join (,) <$> valGen
    GTOther g  -> join (,) <$> g
    GTMaybe g  -> maybe (Nothing, Nothing) (bimap Just Just) <$>
        Gen.maybe (runGT g)
    g1 :&: g2  -> do
      (x1, y1) <- runGT g1
      (x2, y2) <- runGT g2
      pure ((x1,x2), (y1,y2))
    GTNEList r g -> first toList . NE.unzip <$>
        Gen.nonEmpty (fromMaybe mapSize r) (runGT g)
    GTSet g -> bimap S.fromList S.fromList . unzip <$>
        Gen.list mapSize (runGT g)
    GTSorted s g -> bimap (runSorter s) (fromJust . NE.nonEmpty . runSorter s . toList) <$>
                      runGT g

runTT :: Monad m => TestType a b -> a -> b -> PropertyT m ()
runTT = \case
    TTNEMap -> \x y -> do
      annotate $ show y
      assert $ NEM.valid y
      x === NEM.IsNonEmpty y
    TTMap   -> (===)
    TTKey   -> (===)
    TTVal   -> (===)
    TTOther -> (===)
    TTThese t1 t2 -> \(x1, x2) -> \case
      This y1 -> do
        runTT t1 x1 y1
        x2 === mempty
      That y2 -> do
        x1 === mempty
        runTT t2 x2 y2
      These y1 y2 -> do
        runTT t1 x1 y1
        runTT t2 x2 y2
    TTMThese t1 t2 -> \(x1, x2) -> \case
      Nothing -> do
        x1 === mempty
        x2 === mempty
      Just (This y1) -> do
        runTT t1 x1 y1
        x2 === mempty
      Just (That y2) -> do
        x1 === mempty
        runTT t2 x2 y2
      Just (These y1 y2) -> do
        runTT t1 x1 y1
        runTT t2 x2 y2
    TTMaybe tt -> \x y -> do
      isJust y === isJust y
      traverse_ (uncurry (runTT tt)) $ liftA2 (,) x y
    TTNEList tt -> \xs ys -> do
      length xs === length ys
      zipWithM_ (runTT tt) xs (toList ys)
    t1 :*: t2 -> \(x1, x2) (y1, y2) -> do
      runTT t1 x1 y1
      runTT t2 x2 y2
    gt :-> tt -> \f g -> do
      (x, y) <- forAll $ runGT gt
      runTT tt (f x) (g y)

ttProp :: TestType a b -> a -> b -> Property
ttProp tt x = property . runTT tt x




combiner :: Integer -> T.Text -> T.Text -> T.Text
combiner n v u
    | even (T.length v) = T.reverse v <> u
    | even n            = v <> u
    | otherwise         = u <> v
  
adjuster :: Integer -> T.Text -> T.Text
adjuster i
    | even i    = T.reverse
    | otherwise = T.intersperse '_'

updater :: Integer -> T.Text -> Maybe T.Text
updater i
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
        let k = maybe 0 (subtract 1 . fst) $ M.lookupMin m
        v <- valGen
        pure $ NEM.insertMinMap k v m
    assert $ M.valid n

prop_valid_insertMaxMap :: Property
prop_valid_insertMaxMap = property $ do
    n  <- forAll $ do
        m <- mapGen
        let k = maybe 0 ((+ 1) . fst) $ M.lookupMax m
        v <- valGen
        pure $ NEM.insertMaxMap k v m
    assert $ M.valid n

prop_valid_insertMapMin :: Property
prop_valid_insertMapMin = property $ do
    n  <- forAll $ do
        m <- mapGen
        let k = maybe 0 (subtract 1 . fst) $ M.lookupMin m
        v <- valGen
        pure $ NEM.insertMapMin k v m
    assert $ NEM.valid n

prop_valid_insertMapMax :: Property
prop_valid_insertMapMax = property $ do
    n  <- forAll $ do
        m <- mapGen
        let k = maybe 0 ((+ 1) . fst) $ M.lookupMax m
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

--   , alterF
--   , alterF'

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
    (M.unionWith (combiner 0))
    (NEM.unionWith (combiner 0))

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
    (M.unionsWith (combiner 0))
    (NEM.unionsWith (combiner 0))

prop_difference :: Property
prop_difference = ttProp (GTNEMap :-> GTNEMap :-> TTMap)
    M.difference
    NEM.difference

prop_differenceWithKey :: Property
prop_differenceWithKey = ttProp (GTNEMap :-> GTNEMap :-> TTMap)
    (M.differenceWithKey f)
    (NEM.differenceWithKey f)
  where
    f n v u
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

  -- , traverseWithKey1
  -- , traverseWithKey
  -- , traverseMaybeWithKey
  -- , traverseMaybeWithKey1

prop_mapAccumWithKey :: Property
prop_mapAccumWithKey = ttProp  ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                             :-> GTNEMap
                             :-> TTOther :*: TTNEMap
                               )
    (M.mapAccumWithKey   f)
    (NEM.mapAccumWithKey f)
  where
    f d i t
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
    f d i t
      | even i    = (sin d, t <> T.pack (show t))
      | otherwise = (d + 2, T.reverse t         )

prop_mapKeys :: Property
prop_mapKeys = ttProp (GTNEMap :-> TTNEMap)
    (M.mapKeys   f)
    (NEM.mapKeys f)
  where
    f = (`mod` 25)
  
prop_mapKeysWith :: Property
prop_mapKeysWith = ttProp (GTNEMap :-> TTNEMap)
    (M.mapKeysWith   (combiner 0) f)
    (NEM.mapKeysWith (combiner 0) f)
  where
    f = (`mod` 25)
  
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
    (foldr1     (combiner 0))
    (NEM.foldr1 (combiner 0))
  
prop_foldl1 :: Property
prop_foldl1 = ttProp ( GTNEMap
                   :-> TTOther
                     )
    (foldl1     (combiner 0))
    (NEM.foldl1 (combiner 0))
  
prop_foldrWithKey :: Property
prop_foldrWithKey = ttProp ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                         :-> GTNEMap
                         :-> TTOther
                           )
    (M.foldrWithKey   f)
    (NEM.foldrWithKey f)
  where
    f i t d
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
    f d i t
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
    (foldr1      (combiner 0))
    (NEM.foldr1' (combiner 0))
  
prop_foldl1' :: Property
prop_foldl1' = ttProp ( GTNEMap
                    :-> TTOther
                      )
    (foldl1      (combiner 0))
    (NEM.foldl1' (combiner 0))
  
prop_foldrWithKey' :: Property
prop_foldrWithKey' = ttProp ( GTOther (Gen.double (Range.linearFrac (-1) 1))
                          :-> GTNEMap
                          :-> TTOther
                            )
    (M.foldrWithKey'   f)
    (NEM.foldrWithKey' f)
  where
    f i t d
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
    f d i t
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
    f i = even . (+ fromIntegral i) . T.length

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
    f i = even . (+ fromIntegral i) . T.length
    
prop_takeWhileAntitone :: Property
prop_takeWhileAntitone = ttProp (GTNEMap :-> TTMap)
    (M.takeWhileAntitone   (< 0))
    (NEM.takeWhileAntitone (< 0))

prop_dropWhileAntitone :: Property
prop_dropWhileAntitone = ttProp (GTNEMap :-> TTMap)
    (M.dropWhileAntitone   (< 0))
    (NEM.dropWhileAntitone (< 0))

prop_spanAntitone :: Property
prop_spanAntitone = ttProp (GTNEMap :-> TTThese TTNEMap TTNEMap)
    (M.spanAntitone   (< 0))
    (NEM.spanAntitone (< 0))

prop_mapMaybeWithKey :: Property
prop_mapMaybeWithKey = ttProp (GTNEMap :-> TTMap)
    (M.mapMaybeWithKey   f)
    (NEM.mapMaybeWithKey f)
  where
    f i | even i    = Just . T.length
        | otherwise = const Nothing

prop_mapEitherWithKey :: Property
prop_mapEitherWithKey = ttProp (GTNEMap :-> TTThese TTNEMap TTNEMap)
    (M.mapEitherWithKey   f)
    (NEM.mapEitherWithKey f)
  where
    f i | even i    = Right . T.reverse
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





-- ---------------------
-- Generators
-- ---------------------

keyGen :: MonadGen m => m Integer
keyGen = Gen.integral (Range.linear 0 50)

valGen :: MonadGen m => m T.Text
valGen = Gen.text (Range.linear 0 5) Gen.alphaNum

mapSize :: Range Int
mapSize = Range.exponential 4 8

mapGen :: MonadGen m => m (Map Integer T.Text)
mapGen = Gen.map mapSize $ (,) <$> keyGen <*> valGen

neMapGen :: MonadGen m => m (NEMap Integer T.Text)
neMapGen = Gen.just $ NEM.nonEmptyMap <$> mapGen

mapTests :: Group
mapTests = $$(discover)

