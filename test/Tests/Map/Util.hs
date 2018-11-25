{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Map.Util (
    SortType(..)
  , GenType(..)
  , TestType(..)
  , ttProp
  , Context(..)
  , Bazaar(..)
  , keyGen, valGen, mapSize, mapGen, neMapGen
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Functor.Apply
import           Data.Foldable
import           Data.Kind
import           Data.List.NonEmpty            (NonEmpty(..))
import           Data.Map                      (Map)
import           Data.Map.NonEmpty             (NEMap)
import           Data.Maybe
import           Data.These
import           Hedgehog
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import qualified Data.Map.NonEmpty             as NEM
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

data Context a b t = Context (b -> t) a
    deriving Functor

data Bazaar a b t = Done t
                  | More a (Bazaar a b (b -> t))
    deriving Functor

instance Apply (Bazaar a b) where
    liftF2 f = \case
      Done x   -> fmap (f x)
      More x b -> More x . liftA2 (\g r y -> f (g y) r) b

instance Applicative (Bazaar a b) where
    pure   = Done
    liftA2 = liftF2

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
    TTCtx    :: TestType (c -> t)          (d -> u)
             -> TestType a                 b
             -> TestType (Context a c t)   (Context b d u)
    TTBazaar :: (Show a, Show b, Show c, Show d)
             => GenType  c                 d
             -> TestType t                 u
             -> TestType a                 b
             -> TestType (Bazaar a c t)    (Bazaar b d u)
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
    TTCtx tSet tView -> \(Context xS xV) (Context yS yV) -> do
      runTT tSet  xS yS
      runTT tView xV yV
    TTBazaar gNew tRes tView -> testBazaar gNew tRes tView
    t1 :*: t2 -> \(x1, x2) (y1, y2) -> do
      runTT t1 x1 y1
      runTT t2 x2 y2
    gt :-> tt -> \f g -> do
      (x, y) <- forAll $ runGT gt
      runTT tt (f x) (g y)

-- testBazaar'
--     :: forall a b c d t u m. (Show c, Show d, Monad m)
--     => Gen (c, d)
--     -> (t -> u -> PropertyT m ())
--     -> (a -> b -> PropertyT m ())
--     -> Bazaar a c t
--     -> Bazaar b d u
--     -> PropertyT m ()
-- testBazaar' gCD tRes0 tView = go tRes0
--   where
--     go  :: (t' -> u' -> PropertyT m ()) -> Bazaar a c t' -> Bazaar b d u' -> PropertyT m ()
--     go tRes = \case
--       Done xRes -> \case
--         Done yRes ->
--           tRes xRes yRes
--         More _ _ ->
--           failure
--       More xView xNext -> \case
--         Done _ ->
--           failure
--         More yView yNext -> do
--           tView xView yView
--           let tRes' f g = do
--                 (xNew, yNew) <- forAll gCD
--                 tRes (f xNew) (g yNew)
--           go tRes' xNext yNext

-- testBazaar2
--     :: forall a b c d t u m. (Show c, Show d, Monad m)
--     => GenType  c d
--     -> TestType t u
--     -> TestType a b
--     -> Bazaar a c t
--     -> Bazaar b d u
--     -> PropertyT m ()
-- testBazaar2 gCD tRes tView = testBazaar' (runGT gCD) (runTT tRes) (runTT tView)

testBazaar
    :: forall a b c d t u m. (Show a, Show b, Show c, Show d, Monad m)
    => GenType  c d
    -> TestType t u
    -> TestType a b
    -> Bazaar a c t
    -> Bazaar b d u
    -> PropertyT m ()
testBazaar gNew tRes0 tView = go [] [] tRes0
  where
    go  :: [a] -> [b] -> TestType t' u' -> Bazaar a c t' -> Bazaar b d u' -> PropertyT m ()
    go xs ys tRes = \case
      Done xRes -> \case
        Done yRes -> do
          annotate "The final result matches"
          runTT tRes xRes yRes
        More yView _ -> do
          annotate "ys had more elements than xs"
          annotate $ show xs
          annotate $ show ys
          annotate $ show yView
          failure
      More xView xNext -> \case
        Done _ -> do
          annotate "xs had more elements than ys"
          annotate $ show xs
          annotate $ show ys
          annotate $ show xView
          failure
        More yView yNext -> do
          annotate "Each individual piece matches pair-wise"
          runTT tView xView yView
          annotate "The remainders also match"
          go (xView:xs) (yView:ys) (gNew :-> tRes) xNext yNext


ttProp :: TestType a b -> a -> b -> Property
ttProp tt x = property . runTT tt x

-- ---------------------
-- Generators
-- ---------------------

keyGen :: MonadGen m => m Integer
keyGen = Gen.integral (Range.linear (-100) 100)

valGen :: MonadGen m => m T.Text
valGen = Gen.text (Range.linear 0 5) Gen.alphaNum

mapSize :: Range Int
mapSize = Range.exponential 4 8

mapGen :: MonadGen m => m (Map Integer T.Text)
mapGen = Gen.map mapSize $ (,) <$> keyGen <*> valGen

neMapGen :: MonadGen m => m (NEMap Integer T.Text)
neMapGen = Gen.just $ NEM.nonEmptyMap <$> mapGen
