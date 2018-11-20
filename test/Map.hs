{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Map (mapTests) where

import           Control.Monad
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.NonEmpty  (NEMap, Map(..))
import           Hedgehog
import           System.Exit
import           System.IO
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import qualified Data.Map.NonEmpty  as NEM
import qualified Data.Set           as S
import qualified Data.Text          as T
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range

mapTests :: Group
mapTests = $$(discover)

mapSize :: Range Int
mapSize = Range.exponential 4 256

genKey :: MonadGen m => m Int
genKey = Gen.int  (Range.linear (-10000) 10000)

genVal :: MonadGen m => m T.Text
genVal = Gen.text (Range.singleton 5) Gen.alphaNum

genMap :: MonadGen m => m (Map Int T.Text)
genMap = Gen.map mapSize $ (,) <$> genKey <*> genVal

genNEMap :: MonadGen m => m (NEMap Int T.Text)
genNEMap = Gen.just $ NEM.nonEmptyMap <$> genMap

combiner :: Int -> T.Text -> T.Text -> T.Text
combiner n v u
    | even n    = v <> u
    | otherwise = u <> v

prop_valid :: Property
prop_valid = property $
    assert . NEM.valid =<< forAll genNEMap

prop_valid_insertMap :: Property
prop_valid_insertMap = property $
    assert . NEM.valid =<< forAll (NEM.insertMap <$> genKey <*> genVal <*> genMap)

prop_valid_insertMapMin :: Property
prop_valid_insertMapMin = property $ do
    n  <- forAll $ do
        m <- genMap
        let k = maybe 0 (subtract 1 . fst) $ M.lookupMin m
        v <- genVal
        pure $ NEM.insertMapMin k v m
    assert $ NEM.valid n

prop_valid_insertMapMax :: Property
prop_valid_insertMapMax = property $ do
    n  <- forAll $ do
        m <- genMap
        let k = maybe 0 ((+ 1) . fst) $ M.lookupMax m
        v <- genVal
        pure $ NEM.insertMapMax k v m
    assert $ NEM.valid n

mapMapMatch
    :: (Ord k, Show k, Eq a, Show a)
    -- => Gen (M.Map k a, NEM.NEMap k a)
    => PropertyT IO (M.Map k a, NEM.NEMap k a)
    -> Property
mapMapMatch g = property $ do
    (m, n) <- g
    assert $ NEM.valid n
    m === IsNonEmpty n

prop_insertMapWithKey :: Property
prop_insertMapWithKey = mapMapMatch $ do
    m <- forAll genMap
    k <- forAll genKey
    v <- forAll genVal
    let m0 = M.insertWithKey combiner k v m
        m1 = NEM.insertMapWithKey combiner k v m
    pure (m0, m1)

prop_singleton :: Property
prop_singleton = mapMapMatch $ do
    k <- forAll genKey
    v <- forAll genVal
    let m0 = M.singleton k v
        m1 = NEM.singleton k v
    pure (m0, m1)

-- TODO: custom sample monad

prop_fromListWithKey :: Property
prop_fromListWithKey = mapMapMatch $ do
    kvs <- forAll $ Gen.nonEmpty mapSize $ (,) <$> genKey <*> genVal
    let m0 = M.fromListWithKey   combiner (toList kvs)
        m1 = NEM.fromListWithKey combiner kvs
    pure (m0, m1)

prop_fromAscListWithKey :: Property
prop_fromAscListWithKey = mapMapMatch $ do
    kvs <- forAll . Gen.just . fmap (NE.nonEmpty . S.toAscList) $        -- TODO: use nonempty set
                Gen.set mapSize $ (,) <$> genKey <*> genVal
    let m0 = M.fromAscListWithKey   combiner (toList kvs)
        m1 = NEM.fromAscListWithKey combiner kvs
    pure (m0, m1)

prop_fromDescListWithKey :: Property
prop_fromDescListWithKey = mapMapMatch $ do
    kvs <- forAll . Gen.just . fmap (NE.nonEmpty . S.toDescList) $       -- TODO: use nonempty set
                Gen.set mapSize $ (,) <$> genKey <*> genVal
    let m0 = M.fromDescListWithKey   combiner (toList kvs)
        m1 = NEM.fromDescListWithKey combiner kvs
    pure (m0, m1)

prop_fromDistinctAscList :: Property
prop_fromDistinctAscList = mapMapMatch $ do
    kvs <- forAll $ NEM.toAscList <$> genNEMap
    let m0 = M.fromDistinctAscList   (toList kvs)
        m1 = NEM.fromDistinctAscList kvs
    pure (m0, m1)

prop_fromDistinctDescList :: Property
prop_fromDistinctDescList = mapMapMatch $ do
    kvs <- forAll $ NEM.toDescList <$> genNEMap
    let m0 = M.fromDistinctDescList   (toList kvs)
        m1 = NEM.fromDistinctDescList kvs
    pure (m0, m1)

