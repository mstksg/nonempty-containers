{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module Tests.Util (
    K(..), KeyType, overKX, dummyKey
  , SortType(..)
  , GenFunc(..), gf1, gf2, gf3, gf4
  , GenType(..)
  , TestType(..)
  , ttProp
  , groupTree
  , readShow, readShow1, showShow1, showShow2
  , Context(..)
  , Bazaar(..)
  , keyGen, valGen, mapSize, mapGen, neMapGen, setGen, neSetGen
  , intKeyGen, intMapGen, neIntMapGen, intSetGen, neIntSetGen
  , seqGen, neSeqGen
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable
import           Data.Function
import           Data.Functor.Apply
import           Data.Functor.Classes
import           Data.Functor.Identity
import           Data.IntMap                (IntMap)
import           Data.IntMap.NonEmpty       (NEIntMap)
import           Data.IntSet                (IntSet, Key)
import           Data.IntSet.NonEmpty       (NEIntSet)
import           Data.Kind
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Map                   (Map)
import           Data.Map.NonEmpty          (NEMap)
import           Data.Maybe
import           Data.Semigroup.Foldable
import           Data.Sequence              (Seq(..))
import           Data.Sequence.NonEmpty     (NESeq(..))
import           Data.Set                   (Set)
import           Data.Set.NonEmpty          (NESet)
import           Data.Text                  (Text)
import           Data.These
import           Hedgehog
import           Hedgehog.Function hiding   ((:*:))
import           Hedgehog.Internal.Property
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Text.Read
import qualified Data.IntMap                as IM
import qualified Data.IntMap.NonEmpty       as NEIM
import qualified Data.IntSet                as IS
import qualified Data.IntSet.NonEmpty       as NEIS
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Map.NonEmpty          as NEM
import qualified Data.Sequence.NonEmpty     as NESeq
import qualified Data.Set                   as S
import qualified Data.Set.NonEmpty          as NES
import qualified Data.Text                  as T
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup             (Semigroup(..))
#endif

groupTree :: Group -> TestTree
groupTree Group{..} = testGroup (unGroupName groupName)
                                (map (uncurry go) groupProperties)
  where
    go :: PropertyName -> Property -> TestTree
    go n = testProperty (mkName (unPropertyName n))
    mkName = map deUnderscore . drop (length @[] @Char "prop_")
    deUnderscore '_' = ' '
    deUnderscore c   = c

-- | test for stability
data K a b = K { getKX :: !a, getKY :: !b }
    deriving (Show, Read, Generic)

withK :: (a -> b -> c) -> K a b -> c
withK f (K x y) = f x y

overKX :: (a -> c) -> K a b -> K c b
overKX f (K x y) = K (f x) y

instance Eq a => Eq (K a b) where
    (==) = (==) `on` getKX

instance Ord a => Ord (K a b) where
    compare = compare `on` getKX

instance (Vary a, Vary b) => Vary (K a b)
instance (Arg a, Arg b) => Arg (K a b)

type KeyType = K Int Text

instance Semigroup KeyType where
    K x1 y1 <> K x2 y2 = K (x1 + x2) (y1 <> y2)

instance Monoid KeyType where
    mempty = K 0 ""
    mappend = (<>)

dummyKey :: KeyType
dummyKey = K 0 "hello"


#if MIN_VERSION_base(4,11,0)
instance (Num a, Monoid b) => Num (K a b) where
#else
instance (Num a, Semigroup b, Monoid b) => Num (K a b) where
#endif
    K x1 y1 + K x2 y2 = K (x1 + x2) (y1 <> y2)
    K x1 y1 - K x2 y2 = K (x1 - x2) (y1 <> y2)
    K x1 y1 * K x2 y2 = K (x1 * x2) (y1 <> y2)
    negate (K x y)    = K (negate x) y
    abs    (K x y)    = K (abs x)    y
    signum (K x y)    = K (signum x) y
    fromInteger n     = K (fromInteger n) mempty

data Context a b t = Context (b -> t) a
    deriving Functor

data Bazaar a b t = Done t
                  | More a (Bazaar a b (b -> t))
    deriving Functor

instance Apply (Bazaar a b) where
#if MIN_VERSION_semigroupoids(5,2,2)
    liftF2 f = \case
      Done x   -> fmap (f x)
      More x b -> More x . liftA2 (\g r y -> f (g y) r) b
#else
    (<.>) = \case
        Done x   -> fmap x
        More x b -> More x . liftA2 (\g r y -> g y r) b
#endif

instance Applicative (Bazaar a b) where
    pure   = Done
    liftA2 = liftF2

data SortType :: Type -> Type where
    STAsc          :: Ord a => SortType a
    STDesc         :: Ord a => SortType a
    STDistinctAsc  :: Ord a => SortType (a, b)
    STDistinctDesc :: Ord a => SortType (a, b)

data GenType :: Type -> Type -> Type where
    GTNEMap     :: GenType (Map KeyType Text) (NEMap KeyType Text)
    GTMap       :: GenType (Map KeyType Text) (Map KeyType Text  )
    GTNESet     :: GenType (Set KeyType     ) (NESet KeyType     )
    GTNEIntMap  :: GenType (IntMap Text     ) (NEIntMap Text     )
    GTNEIntSet  :: GenType IntSet             NEIntSet
    GTIntMap    :: GenType (IntMap Text     ) (IntMap Text       )
    GTNESeq     :: GenType (Seq Text        ) (NESeq Text        )
    GTNESeqList :: GenType (NonEmpty Text   ) (NESeq Text        )
    GTSeq       :: GenType (Seq Text        ) (Seq Text          )
    GTKey       :: GenType KeyType            KeyType
    GTIntKey    :: GenType Int                Int
    GTVal       :: GenType Text               Text
    GTSize      :: GenType Int                Int
    GTOther     :: Gen a
                -> GenType a                  a
    GTMaybe     :: GenType a                  b
                -> GenType (Maybe a)          (Maybe b)
    (:&:)       :: GenType a                  b
                -> GenType c                  d
                -> GenType (a, c)             (b, d)
    GTNEList    :: Maybe (Range Int)
                -> GenType a                  b
                -> GenType [a]                (NonEmpty b)
    GTSet       :: GenType (Set KeyType)      (Set KeyType)
    GTIntSet    :: GenType IntSet             IntSet
    GTSorted    :: SortType a
                -> GenType [a]                (NonEmpty a)
                -> GenType [a]                (NonEmpty a)

data GenFunc :: Type -> Type -> Type -> Type where
    GF  :: (Show a, Arg a, Vary a, Show b)
        => Gen b
        -> ((a -> b) -> f)
        -> GenFunc f c d

gf1 :: (Show a, Arg a, Vary a, Show b)
    => Gen b
    -> GenFunc (a -> b) c d
gf1 = (`GF` id)

gf2 :: (Show a, Show b, Arg a, Vary a, Arg b, Vary b, Show c)
    => Gen c
    -> GenFunc (a -> b -> c) d e
gf2 = (`GF` curry)

gf3 :: (Show a, Show b, Show c, Arg a, Vary a, Arg b, Vary b, Arg c, Vary c, Show d)
    => Gen d
    -> GenFunc (a -> b -> c -> d) e f
gf3 = (`GF` (curry . curry))

gf4 :: (Show a, Show b, Show c, Arg a, Vary a, Arg b, Vary b, Arg c, Vary c, Show d, Show e, Arg d, Vary d)
    => Gen e
    -> GenFunc (a -> b -> c -> d -> e) f g
gf4 = (`GF` (curry . curry . curry))




data TestType :: Type -> Type -> Type where
    TTNEMap     :: (Eq a, Show a)
                => TestType (Map KeyType a) (NEMap KeyType a  )
    TTNEIntMap  :: (Eq a, Show a)
                => TestType (IntMap a     ) (NEIntMap a       )
    TTNESet     :: TestType (Set KeyType  ) (NESet KeyType    )
    TTNEIntSet  :: TestType IntSet          NEIntSet
    TTMap       :: (Eq a, Show a)
                => TestType (Map KeyType a) (Map    KeyType a )
    TTSet       :: TestType (Set KeyType  ) (Set    KeyType   )
    TTNESeq     :: (Eq a, Show a)
                => TestType (Seq a        ) (NESeq a          )
    TTNESeqList :: (Eq a, Show a)
                => TestType (NonEmpty a   ) (NESeq a          )
    TTKey       :: TestType KeyType         KeyType
    TTVal       :: TestType Text            Text
    TTOther     :: (Eq a, Show a)
                => TestType a               a
    TTThese     :: (Eq a, Show a, Monoid a, Eq c, Show c, Monoid c)
                => TestType a               b
                -> TestType c               d
                -> TestType (a, c)          (These b d)
    TTMThese    :: (Eq a, Show a, Monoid a, Eq c, Show c, Monoid c)
                => TestType a               b
                -> TestType c               d
                -> TestType (a, c)          (Maybe (These b d))
    TTTThese    :: (Eq a, Show a, Monoid a, Eq c, Show c, Monoid c, Eq e, Show e, Monoid e)
                => TestType a               b
                -> TestType c               d
                -> TestType e               f
                -> TestType (Maybe a, c, e) (These b (These d f))
    TTMaybe     :: TestType a               b
                -> TestType (Maybe a)       (Maybe b)
    TTEither    :: TestType a               b
                -> TestType c               d
                -> TestType (Either a c)    (Either b d)
    TTNEList    :: TestType a               b
                -> TestType [a]             (NonEmpty b)
    TTCtx       :: TestType (c -> t)        (d -> u)
                -> TestType a               b
                -> TestType (Context a c t) (Context b d u)
    TTBazaar    :: (Show a, Show b, Show c, Show d)
                => GenType  c               d
                -> TestType t               u
                -> TestType a               b
                -> TestType (Bazaar a c t)  (Bazaar b d u)
    (:*:)       :: (Eq a, Eq b, Eq c, Eq d, Show a, Show b, Show c, Show d)
                => TestType a               b
                -> TestType c               d
                -> TestType (a, c)          (b, d)
    (:?>)       :: GenFunc f   c            d
                -> TestType    c            d
                -> TestType    (f -> c)     (f -> d)
    (:->)       :: (Show a, Show b)
                => GenType  a               b
                -> TestType c               d
                -> TestType (a -> c)        (b -> d)

infixr 2 :&:
infixr 1 :->
infixr 1 :?>
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
    GTNEMap     -> (\n -> (NEM.IsNonEmpty n, n)) <$> neMapGen
    GTMap       -> join (,) <$> mapGen
    GTNESet     -> (\n -> (NES.IsNonEmpty  n, n)) <$> neSetGen
    GTNEIntMap  -> (\n -> (NEIM.IsNonEmpty n, n)) <$> neIntMapGen
    GTNEIntSet  -> (\n -> (NEIS.IsNonEmpty n, n)) <$> neIntSetGen
    GTIntMap    -> join (,) <$> intMapGen
    GTSet       -> join (,) <$> setGen
    GTIntSet    -> join (,) <$> intSetGen
    GTNESeq     -> (\n -> (NESeq.IsNonEmpty n, n)) <$> neSeqGen
    GTNESeqList -> (\n -> (toNonEmpty n, n)) <$> neSeqGen
    GTSeq       -> join (,) <$> seqGen
    GTKey       -> join (,) <$> keyGen
    GTIntKey    -> join (,) <$> intKeyGen
    GTVal       -> join (,) <$> valGen
    GTSize      -> join (,) <$> Gen.int mapSize
    GTOther g   -> join (,) <$> g
    GTMaybe g   -> maybe (Nothing, Nothing) (bimap Just Just) <$>
      Gen.maybe (runGT g)
    g1 :&: g2  -> do
      (x1, y1) <- runGT g1
      (x2, y2) <- runGT g2
      pure ((x1,x2), (y1,y2))
    GTNEList r g -> first toList . NE.unzip <$>
        Gen.nonEmpty (fromMaybe mapSize r) (runGT g)
    GTSorted s g -> bimap (runSorter s) (fromJust . NE.nonEmpty . runSorter s . toList) <$>
                      runGT g

runTT :: Monad m => TestType a b -> a -> b -> PropertyT m ()
runTT = \case
    TTNEMap -> \x y -> do
      assert $ NEM.valid y
      unKMap x === unKMap (NEM.IsNonEmpty y)
    TTNEIntMap -> \x y -> do
      assert $ NEIM.valid y
      x === NEIM.IsNonEmpty y
    TTNESet -> \x y -> do
      assert $ NES.valid y
      unKSet x === unKSet (NES.IsNonEmpty y)
    TTNEIntSet -> \x y -> do
      assert $ NEIS.valid y
      x === NEIS.IsNonEmpty y
    TTMap   -> \x y ->
      unKMap x === unKMap y
    TTSet   -> \x y ->
      unKSet x === unKSet y
    TTNESeq -> \x y ->
      x === NESeq.IsNonEmpty y
    TTNESeqList -> \x y ->
      x === toNonEmpty y
    TTKey   -> \(K x1 y1) (K x2 y2) -> do
      x1 === x2
      y1 === y2
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
    TTTThese t1 t2 t3 -> \(x1,x2,x3) -> \case
      This y1 -> do
        mapM_ (flip (runTT t1) y1) x1
        x2 === mempty
        x3 === mempty
      That     y23 -> do
        x1 === mempty
        runTT (TTThese t2 t3) (x2, x3) y23
      These y1 y23 -> do
        mapM_ (flip (runTT t1) y1) x1
        runTT (TTThese t2 t3) (x2, x3) y23
    TTMaybe tt -> \x y -> do
      isJust y === isJust y
      traverse_ (uncurry (runTT tt)) $ liftA2 (,) x y
    TTEither tl tr -> \case
      Left x  -> \case
        Left y  -> runTT tl x y
        Right _ -> annotate "Left -> Right" *> failure
      Right x -> \case
        Left _  -> annotate "Right -> Left" *> failure
        Right y -> runTT tr x y
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
    GF gt c :?> tt -> \gx gy -> do
      f <- c <$> forAllFn (fn gt)
      runTT tt (gx f) (gy f)
    gt :-> tt -> \f g -> do
      (x, y) <- forAll $ runGT gt
      runTT tt (f x) (g y)
  where
    unKMap :: (Ord k, Ord j) => Map (K k j) c -> Map (k, j) c
    unKMap = M.mapKeys (withK (,))
    unKSet :: (Ord k, Ord j) => Set (K k j) -> Set (k, j)
    unKSet = S.map (withK (,))

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


-- ---------------------
-- Properties
-- ---------------------

ttProp :: TestType a b -> a -> b -> Property
ttProp tt x = property . runTT tt x

readShow
    :: (Show a, Read a, Eq a)
    => Gen a
    -> Property
readShow g = property $ do
    m0 <- forAll g
    tripping m0 show readMaybe

readShow1
    :: (Eq (f a), Show1 f, Show a, Show (f a), Read1 f, Read a)
    => Gen (f a)
    -> Property
readShow1 g = property $ do
    m0 <- forAll g
    tripping m0 (($ "")  . showsPrec1 0) (fmap fst . listToMaybe . readsPrec1 0)

showShow1
    :: (Show1 f, Show a, Show (f a))
    => Gen (f a)
    -> Property
showShow1 g = property $ do
    m0 <- forAll g
    let s0 = show m0
        s1 = showsPrec1 0 m0 ""
    s0 === s1

showShow2
    :: (Show2 f, Show a, Show b, Show (f a b))
    => Gen (f a b)
    -> Property
showShow2 g = property $ do
    m0 <- forAll g
    let s0 = show m0
        s2 = showsPrec2 0 m0 ""
    s0 === s2

-- readShow2
--     :: (Eq (f a b), Show2 f, Show a, Show b, Show (f a b), Read2 f, Read a, Read b)
--     => Gen (f a b)
--     -> Property
-- readShow2 g = property $ do
--     m0 <- forAll g
--     tripping m0 (($ "")  . showsPrec2 0) (fmap fst . listToMaybe . readsPrec2 0)

-- ---------------------
-- Generators
-- ---------------------

keyGen :: MonadGen m => m KeyType
keyGen = K <$> intKeyGen
           <*> Gen.text (Range.linear 0 5) Gen.alphaNum

valGen :: MonadGen m => m Text
valGen = Gen.text (Range.linear 0 5) Gen.alphaNum

mapSize :: Range Int
mapSize = Range.exponential 1 8

mapGen :: MonadGen m => m (Map KeyType Text)
mapGen = Gen.map mapSize $ (,) <$> keyGen <*> valGen

neMapGen :: (MonadGen m, GenBase m ~ Identity) => m (NEMap KeyType Text)
neMapGen = Gen.just $ NEM.nonEmptyMap <$> mapGen

setGen :: MonadGen m => m (Set KeyType)
setGen = Gen.set mapSize keyGen

neSetGen :: (MonadGen m, GenBase m ~ Identity) => m (NESet KeyType)
neSetGen = Gen.just $ NES.nonEmptySet <$> setGen

intKeyGen :: MonadGen m => m Key
intKeyGen = Gen.int (Range.linear (-100) 100)

intMapGen :: MonadGen m => m (IntMap Text)
intMapGen = IM.fromDistinctAscList . M.toList <$> Gen.map mapSize ((,) <$> intKeyGen <*> valGen)

neIntMapGen :: (MonadGen m, GenBase m ~ Identity) => m (NEIntMap Text)
neIntMapGen = Gen.just $ NEIM.nonEmptyMap <$> intMapGen

intSetGen :: MonadGen m => m IntSet
intSetGen = IS.fromDistinctAscList . S.toList <$> Gen.set mapSize intKeyGen

neIntSetGen :: (MonadGen m, GenBase m ~ Identity) => m NEIntSet
neIntSetGen = Gen.just $ NEIS.nonEmptySet <$> intSetGen

seqGen :: MonadGen m => m (Seq Text)
seqGen = Gen.seq mapSize valGen

neSeqGen :: (MonadGen m, GenBase m ~ Identity) => m (NESeq Text)
neSeqGen = Gen.just $ NESeq.nonEmptySeq <$> seqGen





-- ---------------------
-- Orphans
-- ---------------------

instance Arg Char where
    build = via ord chr

instance Arg Text where
    build = via T.unpack T.pack

instance Vary Char where
    vary = contramap ord vary

instance Vary Text where
    vary = contramap T.unpack vary

