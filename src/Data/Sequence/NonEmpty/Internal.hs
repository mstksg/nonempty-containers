{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.Sequence.NonEmpty.Internal (
    NESeq(..)
  , pattern (:<||)
  , pattern (:||>)
  , withNonEmpty
  , toSeq
  , toList
  , singleton
  , length
  , fromList
  , fromFunction
  , index
  , (<|), (><), (|><)
  , foldMapWithIndex
  , traverseWithIndex1
  , tails
  , zip
  , zipWith
  , unzip
  ) where

import           Control.Comonad
import           Control.DeepSeq
import           Control.Monad.Fix
import           Control.Monad.Zip
import           Data.Bifunctor
import           Data.Data
import           Data.Foldable              (Foldable)
import           Data.Functor.Alt
import           Data.Functor.Apply
import           Data.Functor.Bind
import           Data.Functor.Classes
import           Data.Functor.Extend
import           Data.List.NonEmpty         (NonEmpty(..))
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import           Data.Sequence              (Seq(..))
import           Prelude hiding             (length, zipWith, unzip, zip)
import           Text.Read
import qualified Data.Foldable              as F
import qualified Data.Sequence              as Seq

data NESeq a = NESeq { nesHead :: a
                     , nesTail :: !(Seq a)
                     }
  deriving (Functor, Traversable)

pattern (:<||) :: a -> Seq a -> NESeq a
pattern x :<|| xs = NESeq x xs
{-# COMPLETE (:<||) #-}

unsnoc :: NESeq a -> (Seq a, a)
unsnoc (x :<|| (xs :|> y)) = (x :<| xs, y)
unsnoc (x :<|| Empty     ) = (Empty   , x)
{-# INLINE unsnoc #-}

pattern (:||>) :: Seq a -> a -> NESeq a
pattern xs :||> x <- (unsnoc->(!xs, x))
  where
    (x :<| xs) :||> y = x :<|| (xs :|> y)
    Empty      :||> y = y :<|| Empty
{-# COMPLETE (:||>) #-}

infixr 5 `NESeq`
infixr 5 :<||
infixl 5 :||>

instance Show a => Show (NESeq a) where
    showsPrec p xs = showParen (p > 10) $
        showString "fromList (" . shows (toList xs) . showString ")"

instance Read a => Read (NESeq a) where
    readPrec = parens $ prec 10 $ do
        Ident "fromList" <- lexP
        xs <- parens . prec 10 $ readPrec
        return (fromList xs)
    readListPrec = readListPrecDefault

instance Eq a => Eq (NESeq a) where
    xs == ys = length xs == length ys
            && toList xs == toList ys

instance Show1 NESeq where
    liftShowsPrec sp sl d m =
        showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)

instance Read1 NESeq where
    liftReadsPrec _rp readLst p = readParen (p > 10) $ \r -> do
      ("fromList",s) <- lex r
      (xs, t) <- liftReadsPrec _rp readLst 10 s
      pure (fromList xs, t)

instance Eq1 NESeq where
    liftEq eq xs ys = length xs == length ys && liftEq eq (toList xs) (toList ys)

instance Ord1 NESeq where
    liftCompare cmp xs ys = liftCompare cmp (toList xs) (toList ys)

instance Data a => Data (NESeq a) where
    gfoldl f z (x :<|| xs)    = z (:<||) `f` x `f` xs
    gunfold k z _   = k (k (z (:<||)))
    toConstr _      = consConstr
    dataTypeOf _    = seqDataType
    dataCast1       = gcast1

consConstr :: Constr
consConstr  = mkConstr seqDataType ":<||" [] Infix

seqDataType :: DataType
seqDataType = mkDataType "Data.Sequence.NonEmpty.Internal.NESeq" [consConstr]

withNonEmpty :: r -> (NESeq a -> r) -> Seq a -> r
withNonEmpty def f = \case
    x :<| xs -> f (x :<|| xs)
    Empty    -> def
{-# INLINE withNonEmpty #-}

toSeq :: NESeq a -> Seq a
toSeq (x :<|| xs) = x :<| xs
{-# INLINE toSeq #-}

toList :: NESeq a -> NonEmpty a
toList (x :<|| xs) = x :| F.toList xs

singleton :: a -> NESeq a
singleton = (:<|| Seq.empty)
{-# INLINE singleton #-}

length :: NESeq a -> Int
length (_ :<|| xs) = 1 + Seq.length xs
{-# INLINE length #-}

fromList :: NonEmpty a -> NESeq a
fromList (x :| xs) = x :<|| Seq.fromList xs
{-# INLINE fromList #-}

fromFunction :: Int -> (Int -> a) -> NESeq a
fromFunction n f
    | n < 1     = error "NESeq.fromFunction: must take a positive integer argument"
    | otherwise = f 0 :<|| Seq.fromFunction (n - 1) (f . (+ 1))

index :: NESeq a -> Int -> a
index (x :<|| _ ) 0 = x
index (_ :<|| xs) i = xs `Seq.index` (i - 1)
{-# INLINE index #-}

(<|) :: a -> NESeq a -> NESeq a
x <| xs = x :<|| toSeq xs
{-# INLINE (<|) #-}

(><) :: NESeq a -> NESeq a -> NESeq a
(x :<|| xs) >< ys = x :<|| (xs Seq.>< toSeq ys)
{-# INLINE (><) #-}

(|><) :: NESeq a -> Seq a -> NESeq a
(x :<|| xs) |>< ys = x :<|| (xs Seq.>< ys)
{-# INLINE (|><) #-}

infixr 5 <|
infixr 5 ><
infixr 5 |><

foldMapWithIndex :: Semigroup m => (Int -> a -> m) -> NESeq a -> m
foldMapWithIndex f (x :<|| xs) = maybe (f 0 x) (f 0 x <>)
                               . getOption
                               . Seq.foldMapWithIndex (\i -> Option . Just . f (i + 1))
                               $ xs
{-# INLINE foldMapWithIndex #-}

traverseWithIndex1 :: Apply f => (Int -> a -> f b) -> NESeq a -> f (NESeq b)
traverseWithIndex1 f (x :<|| xs) = case runMaybeApply xs' of
    Left  ys -> (:<||)    <$> f 0 x <.> ys
    Right ys -> (:<|| ys) <$> f 0 x
  where
    xs' = Seq.traverseWithIndex (\i -> MaybeApply . Left . f (i+1)) xs
{-# INLINABLE traverseWithIndex1 #-}

tails :: NESeq a -> NESeq (NESeq a)
tails xs@(_ :<|| ys) = withNonEmpty (singleton xs) ((xs <|) . tails) ys
{-# INLINABLE tails #-}

zip :: NESeq a -> NESeq b -> NESeq (a, b)
zip (x :<|| xs) (y :<|| ys) = (x, y) :<|| Seq.zip xs ys
{-# INLINE zip #-}

zipWith :: (a -> b -> c) -> NESeq a -> NESeq b -> NESeq c
zipWith f (x :<|| xs) (y :<|| ys) = f x y :<|| Seq.zipWith f xs ys
{-# INLINE zipWith #-}

unzip :: NESeq (a, b) -> (NESeq a, NESeq b)
unzip ((x, y) :<|| xys) = bimap (x :<||) (y :<||) . Seq.unzip $ xys
{-# INLINE unzip #-}

instance Semigroup (NESeq a) where
    (<>) = (><)
    {-# INLINE (<>) #-}

instance Apply NESeq where
    (f :<|| fs) <.> xs = fxs |>< fsxs
      where
        fxs  = f <$> xs
        fsxs = fs <.> toSeq xs
    {-# INLINABLE (<.>) #-}

instance Applicative NESeq where
    pure = singleton
    {-# INLINE pure #-}
    (<*>) = (<.>)
    {-# INLINABLE (<*>) #-}

instance Alt NESeq where
    (<!>) = (><)
    {-# INLINE (<!>) #-}

instance Bind NESeq where
    NESeq x xs >>- f = withNonEmpty (f x) ((f x ><) . (>>- f)) xs
    {-# INLINABLE (>>-) #-}

instance Monad NESeq where
    return = pure
    {-# INLINE return #-}
    (>>=) = (>>-)
    {-# INLINABLE (>>=) #-}

instance Extend NESeq where
    duplicated = tails
    {-# INLINE duplicated #-}
    extended f xs0@(_ :<|| xs) = withNonEmpty (singleton (f xs0))
                                              ((f xs0 <|) . extend f)
                                              xs
    {-# INLINE extended #-}

instance Comonad NESeq where
    extract (x :<|| _) = x
    {-# INLINE extract #-}
    duplicate = duplicated
    {-# INLINE duplicate #-}
    extend = extended
    {-# INLINE extend #-}

instance Foldable NESeq where
    fold (x :<|| xs) = x <> F.fold xs
    {-# INLINE fold #-}
    foldMap f (x :<|| xs) = f x <> F.foldMap f xs
    {-# INLINE foldMap #-}
    foldr f z (x :<|| xs) = x `f` foldr f z xs
    {-# INLINE foldr #-}
    foldr' f z (xs :||> x) = F.foldr' f y xs
      where
        !y = f x z
    {-# INLINE foldr' #-}
    foldl f z (xs :||> x) = foldl f z xs `f` x
    {-# INLINE foldl #-}
    foldl' f z (x :<|| xs) = F.foldl' f y xs
      where
        !y = f z x
    {-# INLINE foldl' #-}
    foldr1 f (xs :||> x) = foldr f x xs
    {-# INLINE foldr1 #-}
    foldl1 f (x :<|| xs) = foldl f x xs
    {-# INLINE foldl1 #-}
    null _ = False
    {-# INLINE null #-}
    length = length
    {-# INLINE length #-}

instance Foldable1 NESeq where
    fold1 (x :<|| xs) = maybe x (x <>)
                      . getOption
                      . F.foldMap (Option . Just)
                      $ xs
    {-# INLINE fold1 #-}
    foldMap1 f = foldMapWithIndex (const f)
    {-# INLINE foldMap1 #-}
    toNonEmpty = toList
    {-# INLINE toNonEmpty #-}

instance Traversable1 NESeq where
    traverse1 f = traverseWithIndex1 (const f)
    {-# INLINE traverse1 #-}
    sequence1 (x :<|| xs) = case runMaybeApply xs' of
        Left  ys -> (:<||) <$> x <.> ys
        Right ys -> (:<|| ys) <$> x
      where
        xs' = traverse (MaybeApply . Left) xs
    {-# INLINABLE sequence1 #-}

-- | @mzipWith = zipWith@
--
-- @munzip = unzip@
instance MonadZip NESeq where
    mzipWith = zipWith
    munzip   = unzip

instance MonadFix NESeq where
    mfix = mfixSeq

mfixSeq :: (a -> NESeq a) -> NESeq a
mfixSeq f = fromFunction (length (f err)) (\k -> fix (\xk -> f xk `index` k))
  where
    err = error "mfix for Data.Sequence.NonEmpty.NESeq applied to strict function"

instance NFData a => NFData (NESeq a) where
    rnf (x :<|| xs) = rnf x `seq` rnf xs `seq` ()
