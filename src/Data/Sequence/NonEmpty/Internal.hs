{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.Sequence.NonEmpty.Internal
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Unsafe internal-use functions used in the implementation of
-- "Data.Sequence.NonEmpty".  These functions can potentially be used to
-- break the abstraction of 'NESeq' and produce unsound sequences, so be
-- wary!
module Data.Sequence.NonEmpty.Internal (
  NESeq (..),
  pattern (:<||),
  pattern (:||>),
  withNonEmpty,
  toSeq,
  singleton,
  length,
  fromList,
  fromFunction,
  replicate,
  index,
  (<|),
  (><),
  (|><),
  map,
  foldMapWithIndex,
  traverseWithIndex1,
  tails,
  zip,
  zipWith,
  unzip,
  sortOnSeq,
  unstableSortOnSeq,
  unzipSeq,
  unzipWithSeq,
) where

import Control.Comonad
import Control.DeepSeq
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Zip
import qualified Data.Aeson as A
import Data.Bifunctor
import Data.Coerce
import Data.Data
import qualified Data.Foldable as F
import Data.Functor.Alt
import Data.Functor.Bind
import Data.Functor.Classes
import Data.Functor.Extend
import Data.Functor.Invariant
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Text.Read
import Prelude hiding (length, map, replicate, unzip, zip, zipWith)

{-# ANN module "HLint: ignore Avoid NonEmpty.unzip" #-}

-- | A general-purpose non-empty (by construction) finite sequence type.
--
-- Non-emptiness means that:
--
-- *   Functions that /take/ an 'NESeq' can safely operate on it with the
--     assumption that it has at least value.
-- *   Functions that /return/ an 'NESeq' provide an assurance that the
--     result has at least one value.
--
-- "Data.Sequence.NonEmpty" re-exports the API of "Data.Sequence",
-- faithfully reproducing asymptotics, typeclass constraints, and
-- semantics.  Functions that ensure that input and output maps are both
-- non-empty (like 'Data.Sequence.NonEmpty.<|') return 'NESeq', but
-- functions that might potentially return an empty map (like
-- 'Data.Sequence.NonEmpty.tail') return a 'Seq' instead.
--
-- You can directly construct an 'NESeq' with the API from
-- "Data.Sequence.NonEmpty"; it's more or less the same as constructing
-- a normal 'Seq', except you don't have access to 'Data.Seq.empty'.  There
-- are also a few ways to construct an 'NESeq' from a 'Seq':
--
-- 1.  The 'Data.Sequence.NonEmpty.nonEmptySeq' smart constructor will
--     convert a @'Seq' a@ into a @'Maybe' ('NESeq' a)@, returning 'Nothing' if
--     the original 'Seq' was empty.
-- 2.  You can use 'Data.Sequence.NonEmpty.:<||',
--     'Data.Sequence.NonEmpty.:||>', and
--     'Data.Sequence.NonEmpty.insertSeqAt' to insert a value into a 'Seq'
--     to create a guaranteed 'NESeq'.
-- 3.  You can use the 'Data.Sequence.NonEmpty.IsNonEmpty' and
--     'Data.Sequence.NonEmpty.IsEmpty' patterns to "pattern match" on
--     a 'Seq' to reveal it as either containing a 'NESeq' or an empty
--     sequence.
-- 4.  'Data.Sequence.NonEmpty.withNonEmpty' offers a continuation-based
--     interface for deconstructing a 'Seq' and treating it as if it were an
--     'NESeq'.
--
-- You can convert an 'NESeq' into a 'Seq' with 'toSeq' or
-- 'Data.Sequence.NonEmpty.IsNonEmpty', essentially "obscuring" the
-- non-empty property from the type.
data NESeq a = NESeq
  { nesHead :: a
  , nesTail :: !(Seq a)
  }
  deriving (Traversable, Typeable)

-- | /O(1)/. An abstract constructor for an 'NESeq' that consists of
-- a "head" @a@ and a "tail" @'Seq' a@.  Similar to ':|' for 'NonEmpty'.
--
-- Can be used to match on the head and tail of an 'NESeq', and also used
-- to /construct/ an 'NESeq' by consing an item to the beginnong of
-- a 'Seq', ensuring that the result is non-empty.
pattern (:<||) :: a -> Seq a -> NESeq a
pattern x :<|| xs = NESeq x xs

{-# COMPLETE (:<||) #-}

unsnoc :: NESeq a -> (Seq a, a)
unsnoc (x :<|| (xs :|> y)) = (x :<| xs, y)
unsnoc (x :<|| Empty) = (Empty, x)
{-# INLINE unsnoc #-}

-- | /O(1)/. An abstract constructor for an 'NESeq' that consists of
-- a "init" @'Seq' a@ and a "last" @a@.  Similar to ':|' for 'NonEmpty',
-- but at the end of the list instead of at the beginning.
--
-- Can be used to match on the init and last of an 'NESeq', and also used
-- to /construct/ an 'NESeq' by snocing an item to the end of a 'Seq',
-- ensuring that the result is non-empty.
pattern (:||>) :: Seq a -> a -> NESeq a
pattern xs :||> x <- (unsnoc -> (!xs, x))
  where
    (x :<| xs) :||> y = x :<|| (xs :|> y)
    Empty :||> y = y :<|| Empty

{-# COMPLETE (:||>) #-}

infixr 5 `NESeq`
infixr 5 :<||
infixl 5 :||>

instance Show a => Show (NESeq a) where
  showsPrec p xs =
    showParen (p > 10) $
      showString "fromList (" . shows (toNonEmpty xs) . showString ")"

instance Read a => Read (NESeq a) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- parens . prec 10 $ readPrec
    return (fromList xs)
  readListPrec = readListPrecDefault

instance Eq a => Eq (NESeq a) where
  xs == ys =
    length xs == length ys
      && toNonEmpty xs == toNonEmpty ys

instance Ord a => Ord (NESeq a) where
  compare xs ys = compare (F.toList xs) (F.toList ys)

instance Show1 NESeq where
  liftShowsPrec sp sl d m =
    showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toNonEmpty m)

instance Read1 NESeq where
  liftReadsPrec _rp readLst p = readParen (p > 10) $ \r -> do
    ("fromList", s) <- lex r
    (xs, t) <- liftReadsPrec _rp readLst 10 s
    pure (fromList xs, t)

instance Eq1 NESeq where
  liftEq eq xs ys = length xs == length ys && liftEq eq (toNonEmpty xs) (toNonEmpty ys)

instance Ord1 NESeq where
  liftCompare cmp xs ys = liftCompare cmp (toNonEmpty xs) (toNonEmpty ys)

#if MIN_VERSION_base(4,16,0)
instance Data a => Data (NESeq a) where
  gfoldl f z (x :<|| xs) = z (:<||) `f` x `f` xs
  gunfold k z _ = k (k (z (:<||)))
  toConstr _ = consConstr
  dataTypeOf _ = seqDataType
  dataCast1 = gcast1
#else
#ifndef __HLINT__
instance Data a => Data (NESeq a) where
  gfoldl f z (x :<|| xs) = z (:<||) `f` x `f` xs
  gunfold k z _ = k (k (z (:<||)))
  toConstr _ = consConstr
  dataTypeOf _ = seqDataType
  dataCast1 f = gcast1 f
#endif
#endif

consConstr :: Constr
consConstr = mkConstr seqDataType ":<||" [] Infix

seqDataType :: DataType
seqDataType = mkDataType "Data.Sequence.NonEmpty.Internal.NESeq" [consConstr]

instance A.ToJSON a => A.ToJSON (NESeq a) where
  toJSON = A.toJSON . toSeq
  toEncoding = A.toEncoding . toSeq

instance A.FromJSON a => A.FromJSON (NESeq a) where
  parseJSON =
    withNonEmpty (fail err) pure
      <=< A.parseJSON
    where
      err = "NESeq: Non-empty sequence expected, but empty sequence found"

-- | /O(log n)/. A general continuation-based way to consume a 'Seq' as if
-- it were an 'NESeq'. @'withNonEmpty' def f@ will take a 'Seq'.  If map is
-- empty, it will evaluate to @def@.  Otherwise, a non-empty map 'NESeq'
-- will be fed to the function @f@ instead.
--
-- @'Data.Sequence.NonEmpty.nonEmptySeq' == 'withNonEmpty' 'Nothing' 'Just'@
withNonEmpty :: r -> (NESeq a -> r) -> Seq a -> r
withNonEmpty def f = \case
  x :<| xs -> f (x :<|| xs)
  Empty -> def
{-# INLINE withNonEmpty #-}

-- | /O(1)/.
-- Convert a non-empty sequence back into a normal possibly-empty sequence,
-- for usage with functions that expect 'Seq'.
--
-- Can be thought of as "obscuring" the non-emptiness of the map in its
-- type.  See the 'Data.Sequence.NonEmpty.IsNotEmpty' pattern.
--
-- 'Data.Sequence.NonEmpty.nonEmptySeq' and @'maybe' 'Data.Seq.empty'
-- 'toSeq'@ form an isomorphism: they are perfect structure-preserving
-- inverses of eachother.
toSeq :: NESeq a -> Seq a
toSeq (x :<|| xs) = x :<| xs
{-# INLINE toSeq #-}

-- | \( O(1) \). A singleton sequence.
singleton :: a -> NESeq a
singleton = (:<|| Seq.empty)
{-# INLINE singleton #-}

-- | \( O(1) \). The number of elements in the sequence.
length :: NESeq a -> Int
length (_ :<|| xs) = 1 + Seq.length xs
{-# INLINE length #-}

-- | \( O(n) \). Create a sequence from a finite list of elements.  There
-- is a function 'toNonEmpty' in the opposite direction for all instances
-- of the 'Foldable1' class, including 'NESeq'.
fromList :: NonEmpty a -> NESeq a
fromList (x :| xs) = x :<|| Seq.fromList xs
{-# INLINE fromList #-}

-- | \( O(n) \). Convert a given sequence length and a function representing that
-- sequence into a sequence.
fromFunction :: Int -> (Int -> a) -> NESeq a
fromFunction n f
  | n < 1 = error "NESeq.fromFunction: must take a positive integer argument"
  | otherwise = f 0 :<|| Seq.fromFunction (n - 1) (f . (+ 1))

-- | \( O(\log n) \). @replicate n x@ is a sequence consisting of @n@
-- copies of @x@.  Is only defined when @n@ is positive.
replicate :: Int -> a -> NESeq a
replicate n x
  | n < 1 = error "NESeq.replicate: must take a positive integer argument"
  | otherwise = x :<|| Seq.replicate (n - 1) x
{-# INLINE replicate #-}

-- | \( O(\log(\min(i,n-i))) \). The element at the specified position,
-- counting from 0.  The argument should thus be a non-negative
-- integer less than the size of the sequence.
-- If the position is out of range, 'index' fails with an error.
--
-- prop> xs `index` i = toList xs !! i
--
-- Caution: 'index' necessarily delays retrieving the requested
-- element until the result is forced. It can therefore lead to a space
-- leak if the result is stored, unforced, in another structure. To retrieve
-- an element immediately without forcing it, use 'lookup' or '(!?)'.
index :: NESeq a -> Int -> a
index (x :<|| _) 0 = x
index (_ :<|| xs) i = xs `Seq.index` (i - 1)
{-# INLINE index #-}

-- | \( O(1) \). Add an element to the left end of a non-empty sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(<|) :: a -> NESeq a -> NESeq a
x <| xs = x :<|| toSeq xs
{-# INLINE (<|) #-}

-- | \( O(\log(\min(n_1,n_2))) \). Concatenate two non-empty sequences.
(><) :: NESeq a -> NESeq a -> NESeq a
(x :<|| xs) >< ys = x :<|| (xs Seq.>< toSeq ys)
{-# INLINE (><) #-}

-- | \( O(\log(\min(n_1,n_2))) \). Concatenate a non-empty sequence with
-- a potentially empty sequence ('Seq'), to produce a guaranteed non-empty
-- sequence.  Mnemonic: like '><', but a pipe for the guarunteed non-empty
-- side.
(|><) :: NESeq a -> Seq a -> NESeq a
(x :<|| xs) |>< ys = x :<|| (xs Seq.>< ys)
{-# INLINE (|><) #-}

infixr 5 <|
infixr 5 ><
infixr 5 |><

-- | Defined here but hidden; intended for use with RULES pragma.
map :: (a -> b) -> NESeq a -> NESeq b
map f (x :<|| xs) = f x :<|| fmap f xs
{-# NOINLINE [1] map #-}

{-# RULES
"map/map" forall f g xs. map f (map g xs) = map (f . g) xs
  #-}
{-# RULES
"map/coerce" map coerce = coerce
  #-}

-- | /O(n)/. A generalization of 'foldMap1', 'foldMapWithIndex' takes
-- a folding function that also depends on the element's index, and applies
-- it to every element in the sequence.
foldMapWithIndex :: Semigroup m => (Int -> a -> m) -> NESeq a -> m
#if MIN_VERSION_base(4,11,0)
foldMapWithIndex f (x :<|| xs) = maybe (f 0 x) (f 0 x <>)
                               . Seq.foldMapWithIndex (\i -> Just . f (i + 1))
                               $ xs
#else
foldMapWithIndex f (x :<|| xs) = option (f 0 x) (f 0 x <>)
                               . Seq.foldMapWithIndex (\i -> Option . Just . f (i + 1))
                               $ xs
#endif
{-# INLINE foldMapWithIndex #-}

-- | /O(n)/. 'traverseWithIndex1' is a version of 'traverse1' that also
-- offers access to the index of each element.
traverseWithIndex1 :: Apply f => (Int -> a -> f b) -> NESeq a -> f (NESeq b)
traverseWithIndex1 f (x :<|| xs) = case runMaybeApply xs' of
  Left ys -> (:<||) <$> f 0 x <.> ys
  Right ys -> (:<|| ys) <$> f 0 x
  where
    xs' = Seq.traverseWithIndex (\i -> MaybeApply . Left . f (i + 1)) xs
{-# INLINEABLE traverseWithIndex1 #-}

-- | \( O(n) \).  Returns a sequence of all non-empty suffixes of this
-- sequence, longest first.  For example,
--
-- > tails (fromList (1:|[2,3])) = fromList (fromList (1:|[2,3]) :| [fromList (2:|[3]), fromList (3:|[])])
--
-- Evaluating the \( i \)th suffix takes \( O(\log(\min(i, n-i))) \), but evaluating
-- every suffix in the sequence takes \( O(n) \) due to sharing.

-- TODO: is this true?
tails :: NESeq a -> NESeq (NESeq a)
tails xs@(_ :<|| ys) = withNonEmpty (singleton xs) ((xs <|) . tails) ys
{-# INLINEABLE tails #-}

-- | \( O(\min(n_1,n_2)) \).  'zip' takes two sequences and returns
-- a sequence of corresponding pairs.  If one input is short, excess
-- elements are discarded from the right end of the longer sequence.
zip :: NESeq a -> NESeq b -> NESeq (a, b)
zip (x :<|| xs) (y :<|| ys) = (x, y) :<|| Seq.zip xs ys
{-# INLINE zip #-}

-- | \( O(\min(n_1,n_2)) \).  'zipWith' generalizes 'zip' by zipping with the
-- function given as the first argument, instead of a tupling function.
-- For example, @zipWith (+)@ is applied to two sequences to take the
-- sequence of corresponding sums.
zipWith :: (a -> b -> c) -> NESeq a -> NESeq b -> NESeq c
zipWith f (x :<|| xs) (y :<|| ys) = f x y :<|| Seq.zipWith f xs ys
{-# INLINE zipWith #-}

-- | Unzip a sequence of pairs.
--
-- @
-- unzip ps = ps ``seq`` ('fmap' 'fst' ps) ('fmap' 'snd' ps)
-- @
--
-- Example:
--
-- @
-- unzip $ fromList ((1,"a") :| [(2,"b"), (3,"c")]) =
--   (fromList (1:|[2,3]), fromList ("a":|["b","c"]))
-- @
--
-- See the note about efficiency at 'Data.Sequence.NonEmpty.unzipWith'.
unzip :: NESeq (a, b) -> (NESeq a, NESeq b)
unzip ((x, y) :<|| xys) = bimap (x :<||) (y :<||) . unzipSeq $ xys
{-# INLINE unzip #-}

instance Semigroup (NESeq a) where
  (<>) = (><)
  {-# INLINE (<>) #-}

instance Functor NESeq where
  fmap = map
  {-# INLINE fmap #-}
  x <$ xs = replicate (length xs) x
  {-# INLINE (<$) #-}

-- | @since 0.3.4.4
instance Invariant NESeq where
  invmap f _ = fmap f
  {-# INLINE invmap #-}

instance Apply NESeq where
  (f :<|| fs) <.> xs = fxs |>< fsxs
    where
      fxs = f <$> xs
      fsxs = fs <.> toSeq xs
  {-# INLINEABLE (<.>) #-}

instance Applicative NESeq where
  pure = singleton
  {-# INLINE pure #-}
  (<*>) = (<.>)
  {-# INLINEABLE (<*>) #-}

instance Alt NESeq where
  (<!>) = (><)
  {-# INLINE (<!>) #-}

instance Bind NESeq where
  NESeq x xs >>- f = withNonEmpty (f x) ((f x ><) . (>>- f)) xs
  {-# INLINEABLE (>>-) #-}

instance Monad NESeq where
  return = pure
  {-# INLINE return #-}
  (>>=) = (>>-)
  {-# INLINEABLE (>>=) #-}

instance Extend NESeq where
  duplicated = tails
  {-# INLINE duplicated #-}
  extended f xs0@(_ :<|| xs) =
    withNonEmpty
      (singleton (f xs0))
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

-- | 'foldr1', 'foldl1', 'maximum', and 'minimum' are all total, unlike for
-- 'Seq'.
#if MIN_VERSION_base(4,11,0)
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
#else
instance Foldable NESeq where
    fold (x :<|| xs) = x `mappend` F.fold xs
    {-# INLINE fold #-}
    foldMap f (x :<|| xs) = f x `mappend` F.foldMap f xs
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
#endif

#if MIN_VERSION_base(4,11,0)
instance Foldable1 NESeq where
    fold1 (x :<|| xs) = maybe x (x <>)
                      . F.foldMap Just
                      $ xs
    {-# INLINE fold1 #-}
    foldMap1 f = foldMapWithIndex (const f)
    {-# INLINE foldMap1 #-}
    -- TODO: use build
    toNonEmpty (x :<|| xs) = x :| F.toList xs
    {-# INLINE toNonEmpty #-}
#else
instance Foldable1 NESeq where
    fold1 (x :<|| xs) = option x (x <>)
                      . F.foldMap (Option . Just)
                      $ xs
    {-# INLINE fold1 #-}
    foldMap1 f = foldMapWithIndex (const f)
    {-# INLINE foldMap1 #-}
    -- TODO: use build
    toNonEmpty (x :<|| xs) = x :| F.toList xs
    {-# INLINE toNonEmpty #-}
#endif

instance Traversable1 NESeq where
  traverse1 f = traverseWithIndex1 (const f)
  {-# INLINE traverse1 #-}
  sequence1 (x :<|| xs) = case runMaybeApply xs' of
    Left ys -> (:<||) <$> x <.> ys
    Right ys -> (:<|| ys) <$> x
    where
      xs' = traverse (MaybeApply . Left) xs
  {-# INLINEABLE sequence1 #-}

-- | @mzipWith = zipWith@
--
-- @munzip = unzip@
instance MonadZip NESeq where
  mzipWith = zipWith
  munzip = unzip

instance MonadFix NESeq where
  mfix = mfixSeq

mfixSeq :: (a -> NESeq a) -> NESeq a
mfixSeq f = fromFunction (length (f err)) (\k -> fix (\xk -> f xk `index` k))
  where
    err = error "mfix for Data.Sequence.NonEmpty.NESeq applied to strict function"

instance NFData a => NFData (NESeq a) where
  rnf (x :<|| xs) = rnf x `seq` rnf xs

-- ---------------------------------------------

-- | CPP for new functions not in old containers
-- ---------------------------------------------

-- | Compatibility layer for 'Data.Sequence.sortOn'.
sortOnSeq :: Ord b => (a -> b) -> Seq a -> Seq a
sortOnSeq = Seq.sortOn
{-# INLINE sortOnSeq #-}

-- | Compatibility layer for 'Data.Sequence.unstableSortOn'.
unstableSortOnSeq :: Ord b => (a -> b) -> Seq a -> Seq a
unstableSortOnSeq = Seq.unstableSortOn
{-# INLINE unstableSortOnSeq #-}

-- | Compatibility layer for 'Data.Sequence.unzip'.
unzipSeq :: Seq (a, b) -> (Seq a, Seq b)
unzipSeq = Seq.unzip
{-# INLINE unzipSeq #-}

-- | Compatibility layer for 'Data.Sequence.unzipWith'.
unzipWithSeq :: (a -> (b, c)) -> Seq a -> (Seq b, Seq c)
unzipWithSeq = Seq.unzipWith
{-# INLINE unzipWithSeq #-}
