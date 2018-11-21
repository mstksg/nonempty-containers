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
import           Data.Functor.Identity
import           Data.Kind
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Map                   (Map)
import           Data.Map.NonEmpty          (NEMap)
import           Data.Maybe
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
    GTNEMap  :: GenType (M.Map Int T.Text) (NEM.NEMap Int T.Text)
    GTKey    :: GenType Int                Int
    GTVal    :: GenType T.Text             T.Text
    GTOther  :: Gen a
             -> GenType a                  a
    GTMaybe  :: GenType a                  b
             -> GenType (Maybe a)          (Maybe b)
    (:&:)    :: GenType a                  b
             -> GenType c                  d
             -> GenType (a, c)             (b, d)
    GTNEList :: Maybe (Range Int)
             -> GenType a                  b
             -> GenType [a]                (NonEmpty b)
    GTSorted :: SortType a
             -> GenType [a]                (NonEmpty a)
             -> GenType [a]                (NonEmpty a)
    -- Maybe (SortType a)

data TestType :: Type -> Type -> Type where
    TTNEMap :: TestType (M.Map Int T.Text) (NEM.NEMap Int T.Text)
    TTMap   :: TestType (M.Map Int T.Text) (M.Map     Int T.Text)
    TTKey   :: TestType Int                Int
    TTVal   :: TestType T.Text             T.Text
    TTOther :: (Eq a, Show a)
            => TestType a                  a
    TTMaybe :: (Eq a, Eq b, Show a, Show b)
            => TestType a                  b
            -> TestType (Maybe a)          (Maybe b)
    (:*:)   :: (Eq a, Eq b, Eq c, Eq d, Show a, Show b, Show c, Show d)
            => TestType a                  b
            -> TestType c                  d
            -> TestType (a, c)             (b, d)
    (:->)   :: (Show a, Show b)
            => GenType  a                  b
            -> TestType c                  d
            -> TestType (a -> c)           (b -> d)

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
    GTNEList r g -> first toList . NE.unzip <$> do
        Gen.nonEmpty (fromMaybe mapSize r) (runGT g)
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
    TTMaybe tt -> \x y -> do
      isJust y === isJust y
      traverse_ (uncurry (runTT tt)) $ liftA2 (,) x y
    t1 :*: t2 -> \(x1, x2) (y1, y2) -> do
      runTT t1 x1 y1
      runTT t2 x2 y2
    gt :-> tt -> \f g -> do
      (x, y) <- forAll $ runGT gt
      runTT tt (f x) (g y)

ttProp :: TestType a b -> a -> b -> Property
ttProp tt x = property . runTT tt x

-- data Tester :: Type -> Type where
--     GenKey   :: Tester Int
--     GenVal   :: Tester T.Text
--     GenKeyVals :: Tester (NonEmpty (Int, T.Text))
--     TestFunc :: N.SNatI n
--              => TestType a b
--              -> (V.Vec n (M.Map     Int T.Text) -> a)
--              -> (V.Vec n (NEM.NEMap Int T.Text) -> b)
--              -> Tester ()

-- makeEffect ''Tester

-- runTT :: MonadTest m => TestType a b -> a -> b -> m ()
-- runTT = \case
--     TTNEMap -> \x y -> do
--       assert $ NEM.valid y
--       x === NEM.IsNonEmpty y
--     TTMap   -> (===)
--     TTKey   -> (===)
--     TTVal   -> (===)
--     TTOther -> (===)
--     TTMaybe tt -> \x y -> do
--       isJust y === isJust y
--       traverse_ (uncurry (runTT tt)) $ liftA2 (,) x y
--     TTAnd t1 t2 -> \(x1, x2) (y1, y2) -> do
--       runTT t1 x1 y1
--       runTT t2 x2 y2

-- testerProp :: Eff '[Tester, PropertyT IO] () -> Property
-- testerProp = property . runM . interpretM go
--   where
--     go :: Tester x -> PropertyT IO x
--     go = \case
--       GenKey -> forAll keyGen
--       GenVal -> forAll valGen
--       GenKeyVals -> forAll $
--           Gen.nonEmpty mapSize $ (,) <$> keyGen <*> valGen
--       TestFunc tt f g -> do
--         ns <- sequenceA $ pure (forAll neMapGen)
--         let x = f (NEM.IsNonEmpty <$> ns)
--             y = g ns
--         runTT tt x y

combiner :: Int -> T.Text -> T.Text -> T.Text
combiner n v u
    | even n    = v <> u
    | otherwise = u <> v
  
adjuster :: Int -> T.Text -> T.Text
adjuster i
    | even i    = T.reverse
    | otherwise = T.intersperse '_'

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
    (M.updateWithKey   f)
    (NEM.updateWithKey f)
  where
    f i | even i    = Just . T.reverse
        | otherwise = const Nothing

prop_updateLookupWithKey :: Property
prop_updateLookupWithKey = ttProp (GTKey :-> GTNEMap :-> TTMaybe TTVal :*: TTMap)
    (M.updateLookupWithKey   f)
    (NEM.updateLookupWithKey f)
  where
    f i | even i    = Just . T.reverse
        | otherwise = const Nothing

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
    (M.unionWith (<>))
    (NEM.unionWith (<>))

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
    (M.unionsWith (<>))
    (NEM.unionsWith (<>))

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
prop_mapAccumWithKey = ttProp (GTOther (Gen.bool) :-> GTNEMap :-> TTOther :*: TTNEMap)
    (M.mapAccumWithKey   f)
    (NEM.mapAccumWithKey f)
  where
    f b i t
      | b /= (i `mod` 3 == 0) = (b    , T.reverse t        )
      | otherwise             = (not b, T.intersperse '_' t)

prop_mapAccumRWithKey :: Property
prop_mapAccumRWithKey = ttProp (GTOther (Gen.bool) :-> GTNEMap :-> TTOther :*: TTNEMap)
    (M.mapAccumRWithKey   f)
    (NEM.mapAccumRWithKey f)
  where
    f b i t
      | b /= (i `mod` 3 == 0) = (b    , T.reverse t        )
      | otherwise             = (not b, T.intersperse '_' t)

prop_mapKeys :: Property
prop_mapKeys = ttProp (GTNEMap :-> TTNEMap)
    (M.mapKeys   f)
    (NEM.mapKeys f)
  where
    f = (`mod` 25)
  
prop_mapKeysWith :: Property
prop_mapKeysWith = ttProp (GTNEMap :-> TTNEMap)
    (M.mapKeysWith   (<>) f)
    (NEM.mapKeysWith (<>) f)
  where
    f = (`mod` 25)
  
prop_mapMonotonic :: Property
prop_mapMonotonic = ttProp (GTNEMap :-> TTNEMap)
    (M.mapKeysMonotonic   (* 2))
    (NEM.mapKeysMonotonic (* 2))
  




















keyGen :: MonadGen m => m Int
keyGen = Gen.int  (Range.linear (-500) 500)

valGen :: MonadGen m => m T.Text
valGen = Gen.text (Range.singleton 5) Gen.alphaNum

mapSize :: Range Int
mapSize = Range.exponential 4 256

mapGen :: MonadGen m => m (Map Int T.Text)
mapGen = Gen.map mapSize $ (,) <$> keyGen <*> valGen

neMapGen :: MonadGen m => m (NEMap Int T.Text)
neMapGen = Gen.just $ NEM.nonEmptyMap <$> mapGen

mapTests :: Group
mapTests = $$(discover)

