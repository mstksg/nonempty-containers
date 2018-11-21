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
import           Control.Monad.Freer
import           Control.Monad.Freer.TH
import           Data.Foldable
import           Data.Functor.Identity
import           Data.Kind
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Map                   (Map)
import           Data.Map.NonEmpty          (NEMap)
import           Data.Maybe
import           Data.Vec.Lazy              (Vec(..))
import           Hedgehog
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Map.NonEmpty          as NEM
import qualified Data.Map.NonEmpty.Internal as NEM
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Type.Nat              as N
import qualified Data.Vec.Lazy              as V
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

-- TODO: test functions
data TestType :: Type -> Type -> Type where
    TTNEMap :: TestType (M.Map Int T.Text) (NEM.NEMap Int T.Text)
    TTMap   :: TestType (M.Map Int T.Text) (M.Map     Int T.Text)
    TTKey   :: TestType Int                Int
    TTVal   :: TestType T.Text             T.Text
    TTBool  :: TestType Bool               Bool
    TTOther :: (Eq a, Show a)
            => TestType a                  a
    TTMaybe :: (Eq a, Eq b, Show a, Show b)
            => TestType a                  b
            -> TestType (Maybe a)          (Maybe b)
    -- TTOther :: (Eq r, Show r) => TestType r                  r
    TTAnd   :: (Eq a, Eq b, Eq c, Eq d, Show a, Show b, Show c, Show d)
            => TestType a                  b
            -> TestType c                  d
            -> TestType (a, c)             (b, d)

data Tester :: Type -> Type where
    GenKey   :: Tester Int
    GenVal   :: Tester T.Text
    GenKeyVals :: Tester (NonEmpty (Int, T.Text))
    TestFunc :: N.SNatI n
             => TestType a b
             -> (V.Vec n (M.Map     Int T.Text) -> a)
             -> (V.Vec n (NEM.NEMap Int T.Text) -> b)
             -> Tester ()

makeEffect ''Tester

runTT :: MonadTest m => TestType a b -> a -> b -> m ()
runTT = \case
    TTNEMap -> \x y -> do
      assert $ NEM.valid y
      x === NEM.IsNonEmpty y
    TTMap   -> (===)
    TTKey   -> (===)
    TTVal   -> (===)
    TTBool  -> (===)
    TTOther -> (===)
    TTMaybe tt -> \x y -> do
      isJust y === isJust y
      traverse_ (uncurry (runTT tt)) $ liftA2 (,) x y
    TTAnd t1 t2 -> \(x1, x2) (y1, y2) -> do
      runTT t1 x1 y1
      runTT t2 x2 y2

testerProp :: Eff '[Tester, PropertyT IO] () -> Property
testerProp = property . runM . interpretM go
  where
    go :: Tester x -> PropertyT IO x
    go = \case
      GenKey -> forAll keyGen
      GenVal -> forAll valGen
      GenKeyVals -> forAll $
          Gen.nonEmpty mapSize $ (,) <$> keyGen <*> valGen
      TestFunc tt f g -> do
        ns <- sequenceA $ pure (forAll neMapGen)
        let x = f (NEM.IsNonEmpty <$> ns)
            y = g ns
        runTT tt x y

testFunc0
    :: Member Tester effs
    => TestType a b
    -> a
    -> b
    -> Eff effs ()
testFunc0 tt f g = testFunc @'N.Z tt (const f) (const g)

testFunc1
    :: Member Tester effs
    => TestType a b
    -> (M.Map     Int T.Text -> a)
    -> (NEM.NEMap Int T.Text -> b)
    -> Eff effs ()
testFunc1 tt f g = testFunc @('N.S 'N.Z) tt (f . V.head) (g . V.head)

combiner :: Int -> T.Text -> T.Text -> T.Text
combiner n v u
    | even n    = v <> u
    | otherwise = u <> v

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

-- mapMapMatch
--     :: (Ord k, Show k, Eq a, Show a)
--     => PropertyT IO (M.Map k a, NEM.NEMap k a)
--     -> Property
-- mapMapMatch g = property $ do
--     (m, n) <- g
--     assert $ NEM.valid n
--     m === IsNonEmpty n

-- mapMapMatch'
--     :: (Ord k, Show k, Eq a, Show a)
--     => PropertyT IO (M.Map k a, M.Map k a)
--     -> Property
-- mapMapMatch' g = property $ do
--     (m, n) <- g
--     m === n

prop_insertMapWithKey :: Property
prop_insertMapWithKey = property $ do
    m <- forAll mapGen
    k <- forAll keyGen
    v <- forAll valGen
    let m0 = M.insertWithKey      combiner k v m
        m1 = NEM.insertMapWithKey combiner k v m
    assert $ NEM.valid m1
    m0 === NEM.IsNonEmpty m1


prop_singleton :: Property
prop_singleton = testerProp $ do
    k <- genKey
    v <- genVal
    testFunc0 TTNEMap
        (M.singleton   k v)
        (NEM.singleton k v)

prop_fromAscListWithKey :: Property
prop_fromAscListWithKey = testerProp $ do
    kvs <- send @(PropertyT IO)
         . forAll . Gen.just . fmap (NE.nonEmpty . S.toAscList) $        -- TODO: use nonempty set
                Gen.set mapSize $ (,) <$> keyGen <*> valGen
    testFunc0 TTNEMap
        (M.fromAscListWithKey   combiner (toList kvs))
        (NEM.fromAscListWithKey combiner kvs)

prop_fromDescListWithKey :: Property
prop_fromDescListWithKey = testerProp $ do
    kvs <- send @(PropertyT IO)
         . forAll . Gen.just . fmap (NE.nonEmpty . S.toDescList) $        -- TODO: use nonempty set
                Gen.set mapSize $ (,) <$> keyGen <*> valGen
    testFunc0 TTNEMap
        (M.fromDescListWithKey   combiner (toList kvs))
        (NEM.fromDescListWithKey combiner kvs)

prop_fromDistinctAscList :: Property
prop_fromDistinctAscList = testerProp $ do
    testFunc1 TTNEMap
        (M.fromDistinctAscList   . M.toAscList  )
        (NEM.fromDistinctAscList . NEM.toAscList)

prop_fromDistinctDescList :: Property
prop_fromDistinctDescList = testerProp $ do
    testFunc1 TTNEMap
        (M.fromDistinctDescList   . M.toDescList  )
        (NEM.fromDistinctDescList . NEM.toDescList)

prop_fromListWithKey :: Property
prop_fromListWithKey = testerProp $ do
    kvs <- genKeyVals
    testFunc0 TTNEMap
        (M.fromListWithKey   combiner (toList kvs))
        (NEM.fromListWithKey combiner kvs         )

prop_insert :: Property
prop_insert = testerProp $ do
    k <- genKey
    v <- genVal
    testFunc1 TTNEMap
        (M.insert   k v)
        (NEM.insert k v)

prop_insertWithKey :: Property
prop_insertWithKey = testerProp $ do
    k <- genKey
    v <- genVal
    testFunc1 TTNEMap
        (M.insertWithKey   combiner k v)
        (NEM.insertWithKey combiner k v)

prop_delete :: Property
prop_delete = testerProp $ do
    k <- genKey
    testFunc1 TTMap
        (M.delete   k)
        (NEM.delete k)

prop_adjustWithKey :: Property
prop_adjustWithKey = testerProp $ do
    k <- genKey
    let f i | even i    = T.reverse
            | otherwise = T.intersperse '_'
    testFunc1 TTNEMap
        (M.adjustWithKey   f k)
        (NEM.adjustWithKey f k)

prop_updateWithKey :: Property
prop_updateWithKey = testerProp $ do
    k <- genKey
    let f i | even i    = Just . T.reverse
            | otherwise = const Nothing
    testFunc1 TTMap
        (M.updateWithKey   f k)
        (NEM.updateWithKey f k)

prop_updateLookupWithKey :: Property
prop_updateLookupWithKey = testerProp $ do
    k <- genKey
    let f i | even i    = Just . T.reverse
            | otherwise = const Nothing
    testFunc1 (TTMaybe TTVal `TTAnd` TTMap)
        (M.updateLookupWithKey   f k)
        (NEM.updateLookupWithKey f k)

prop_alter :: Property
prop_alter = testerProp $ do
    k <- genKey
    v <- genVal
    let f t | even (T.length t) = Just $ T.reverse t
            | otherwise         = Nothing
    testFunc1 TTMap
        (M.alter   (f . fromMaybe v) k)
        (NEM.alter (f . fromMaybe v) k)

prop_alter' :: Property
prop_alter' = testerProp $ do
    k <- genKey
    v <- genVal
    let f t | even (T.length t) = T.reverse t
            | otherwise         = T.intersperse '_' t
    testFunc1 TTNEMap
        (M.alter    (Just . f . fromMaybe v) k)
        (NEM.alter' (       f . fromMaybe v) k)

-- --   , alterF
-- --   , alterF'

prop_lookup :: Property
prop_lookup = testerProp $ do
    k <- genKey
    testFunc1 (TTMaybe TTVal)
        (M.lookup   k)
        (NEM.lookup k)

prop_findWithDefault :: Property
prop_findWithDefault = testerProp $ do
    k <- genKey
    v <- genVal
    testFunc1 TTVal
        (M.findWithDefault   v k)
        (NEM.findWithDefault v k)

prop_member :: Property
prop_member = testerProp $ do
    k <- genKey
    testFunc1 TTBool
        (M.member   k)
        (NEM.member k)

prop_notMember :: Property
prop_notMember = testerProp $ do
    k <- genKey
    testFunc1 TTBool
        (M.notMember   k)
        (NEM.notMember k)

prop_lookupLT :: Property
prop_lookupLT = testerProp $ do
    k <- genKey
    testFunc1 (TTMaybe (TTKey `TTAnd` TTVal))
        (M.lookupLT   k)
        (NEM.lookupLT k)

prop_lookupGT :: Property
prop_lookupGT = testerProp $ do
    k <- genKey
    testFunc1 (TTMaybe (TTKey `TTAnd` TTVal))
        (M.lookupGT   k)
        (NEM.lookupGT k)

prop_lookupLE :: Property
prop_lookupLE = testerProp $ do
    k <- genKey
    testFunc1 (TTMaybe (TTKey `TTAnd` TTVal))
        (M.lookupLE   k)
        (NEM.lookupLE k)

prop_lookupGE :: Property
prop_lookupGE = testerProp $ do
    k <- genKey
    testFunc1 (TTMaybe (TTKey `TTAnd` TTVal))
        (M.lookupGE   k)
        (NEM.lookupGE k)

  -- , size

  -- -- * Combine

  -- -- ** Union
  -- , union
  -- , unionWith
  -- , unionWithKey
  -- , unions
  -- , unionsWith

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

