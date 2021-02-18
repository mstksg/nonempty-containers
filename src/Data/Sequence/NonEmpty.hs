{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

-- |
-- Module      : Data.Sequence.NonEmpty
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- = Non-Empty Finite Sequences
--
-- | An @'NESeq' a@ is a non-empty (but finite) sequence of values of type
-- @a@.  Generally has the same interface as 'Data.List.NonEmpty.NonEmpty'.
-- This is a non-empty version of 'Data.Sequence.Seq' from "Data.Sequence".
--
-- The main differences between this type and 'Data.List.NonEmpty.NonEmpty'
-- are:
--
-- *   You cannot have infinite 'NESeq's
-- *   You have constant-time consing from either end, and constant-time
--     unconsing as well (through '<|', '|>', ':<||', and ':||>')
-- *   Concatenation ('><', '|><', '><|') is logarithmic-time.
-- *   You have logarithmic-time indexing and updating at a given index.
--
-- While asymptotics are often better than for 'Data.List.NonEmpty.NonEmpty', there is
-- a decent constant factor involved in most operations.
--
-- See documentation for 'NESeq' for information on how to convert and
-- manipulate such non-empty sequences
--
-- This module essentially re-imports the API of "Data.Sequence.Lazy" and its
-- 'Seq' type, along with semantics and asymptotics.
--
-- Because 'NESeq' is implemented using 'Seq', all of the caveats of using
-- 'Seq' apply.
--
-- All functions take non-empty sequences as inputs.  In situations where
-- their results can be guarunteed to also be non-empty, they also return
-- non-empty maps.  In situations where their results could potentially be
-- empty, 'Seq' is returned instead.
--
-- Some functions (like 'spanl', 'spanr', 'breakl', 'breakr', 'partition',
-- 'splitAt') have modified return types to account for possible
-- configurations of non-emptiness.
--
-- Some functions ('head', 'last', 'tail', 'init') are provided because
-- they are total for non-empty sequences.
--
-- This module is intended to be imported qualified, to avoid name clashes with
-- "Prelude" and "Data.Sequence" functions:
--
-- > import qualified Data.Sequence.NonEmpty as NESeq
module Data.Sequence.NonEmpty (
  -- * Finite sequences
    NESeq ((:<||), (:||>))
  -- ** Conversions between empty and non-empty sequences
  , pattern IsNonEmpty
  , pattern IsEmpty
  , nonEmptySeq
  , toSeq
  , withNonEmpty
  , unsafeFromSeq
  , insertSeqAt
  -- * Construction
  , singleton
  , (<|)
  , (|>)
  , (><)
  , (|><)
  , (><|)
  , fromList
  , fromFunction
  -- ** Repetition
  , replicate
  , replicateA
  , replicateA1
  , replicateM
  , cycleTaking
  -- ** Iterative construction
  , iterateN
  , unfoldr
  , unfoldl
  -- * Deconstruction
  -- | Additional functions for deconstructing sequences are available
  -- via the 'Foldable' instance of 'NESeq'.
  , head
  , tail
  , last
  , init
  -- ** Queries
  , length

  -- * Scans
  , scanl
  , scanl1
  , scanr
  , scanr1
  -- * Sublists
  , tails
  , inits
  , chunksOf
  -- ** Sequential searches
  , takeWhileL
  , takeWhileR
  , dropWhileL
  , dropWhileR
  , spanl
  , spanr
  , breakl
  , breakr
  , partition
  , filter
  -- * Sorting
  , sort
  , sortBy
  , sortOn
  , unstableSort
  , unstableSortBy
  , unstableSortOn
  -- * Indexing
  , lookup
  , (!?)
  , index
  , adjust
  , adjust'
  , update
  , take
  , drop
  , insertAt
  , deleteAt
  , splitAt
  -- ** Indexing with predicates
  -- | These functions perform sequential searches from the left
  -- or right ends of the sequence  returning indices of matching
  -- elements.
  , elemIndexL
  , elemIndicesL
  , elemIndexR
  , elemIndicesR
  , findIndexL
  , findIndicesL
  , findIndexR
  , findIndicesR
  -- * Folds
  -- | General folds are available via the 'Foldable' instance of 'Seq'.
  , foldMapWithIndex
  , foldlWithIndex
  , foldrWithIndex
  -- * Transformations
  , mapWithIndex
  , traverseWithIndex
  , traverseWithIndex1
  , reverse
  , intersperse
  -- ** Zips and unzip
  , zip
  , zipWith
  , zip3
  , zipWith3
  , zip4
  , zipWith4
  , unzip
  , unzipWith
  ) where

import           Control.Applicative
import           Control.Monad hiding            (replicateM)
import           Data.Bifunctor
import           Data.Functor.Apply
import           Data.Sequence                   (Seq(..))
import           Data.Sequence.NonEmpty.Internal
import           Data.These
import           Prelude hiding                  (length, scanl, scanl1, scanr, scanr1, splitAt, zip, zipWith, zip3, zipWith3, unzip, replicate, filter, reverse, lookup, take, drop, head, tail, init, last, map)
import qualified Data.Sequence                   as Seq

-- | /O(1)/. The 'IsNonEmpty' and 'IsEmpty' patterns allow you to treat
-- a 'Seq' as if it were either a @'IsNonEmpty' n@ (where @n@ is a 'NESeq')
-- or an 'IsEmpty'.
--
-- For example, you can pattern match on a 'Seq':
--
-- @
-- safeHead :: 'Seq' Int -> Int
-- safeHead ('IsNonEmpty' (x :<|| _))  = x  -- here, user provided a non-empty sequence, and @n@ is the 'NESeq'
-- safeHead 'IsEmpty'                  = 0  -- here the user provided an empty sequence
-- @
--
-- Matching on @'IsNonEmpty' n@ means that the original 'Seq' was /not/
-- empty, and you have a verified-non-empty 'NESeq' @n@ to use.
--
-- A case statement handling both 'IsNonEmpty' and 'IsEmpty' provides
-- complete coverage.
--
-- This is a bidirectional pattern, so you can use 'IsNonEmpty' to convert
-- a 'NESeq' back into a 'Seq', obscuring its non-emptiness (see 'toSeq').
pattern IsNonEmpty :: NESeq a -> Seq a
pattern IsNonEmpty n <- (nonEmptySeq->Just n)
  where
    IsNonEmpty n = toSeq n

-- | /O(1)/. The 'IsNonEmpty' and 'IsEmpty' patterns allow you to treat
-- a 'Seq' as if it were either a @'IsNonEmpty' n@ (where @n@ is
-- a 'NESeq') or an 'IsEmpty'.
--
-- Matching on 'IsEmpty' means that the original 'Seq' was empty.
--
-- A case statement handling both 'IsNonEmpty' and 'IsEmpty' provides
-- complete coverage.
--
-- This is a bidirectional pattern, so you can use 'IsEmpty' as an
-- expression, and it will be interpreted as 'Data.Seq.empty'.
--
-- See 'IsNonEmpty' for more information.
pattern IsEmpty :: Seq a
pattern IsEmpty <- (Seq.null->True)
  where
    IsEmpty = Seq.empty

{-# COMPLETE IsNonEmpty, IsEmpty #-}

-- | /O(1)/. Smart constructor for an 'NESeq' from a 'Seq'.  Returns
-- 'Nothing' if the 'Seq' was originally actually empty, and @'Just' n@
-- with an 'NESeq', if the 'Seq' was not empty.
--
-- 'nonEmptySeq' and @'maybe' 'Data.Sequence.empty' 'toSeq'@ form an
-- isomorphism: they are perfect structure-preserving inverses of
-- eachother.
--
-- See 'Data.Sequence.NonEmpty.IsNonEmpty' for a pattern synonym that lets
-- you "match on" the possiblity of a 'Seq' being an 'NESeq'.
--
-- > nonEmptySeq (Data.Sequence.fromList [1,2,3]) == Just (fromList (1) :| [2,3])
nonEmptySeq :: Seq a -> Maybe (NESeq a)
nonEmptySeq (x :<| xs) = Just $ x :<|| xs
nonEmptySeq Empty      = Nothing
{-# INLINE nonEmptySeq #-}

-- | /O(1)/. Unsafe version of 'nonEmptySeq'.  Coerces a 'Seq' into an
-- 'NESeq', but is undefined (throws a runtime exception when evaluation is
-- attempted) for an empty 'Seq'.
unsafeFromSeq :: Seq a -> NESeq a
unsafeFromSeq (x :<| xs) = x :<|| xs
unsafeFromSeq Empty      = errorWithoutStackTrace "NESeq.unsafeFromSeq: empty seq"
{-# INLINE unsafeFromSeq #-}

-- | Turn a 'Seq' into a guarantted non-empty 'NESeq' by adding an element
-- at a given index.
--
-- > insertSeqAt 1 0 (Data.Sequence.fromList [1,2,3]) == fromList (1 :| [0,2,3])
insertSeqAt :: Int -> a -> Seq a -> NESeq a
insertSeqAt i y
    | i <= 0    = (y :<||)
    | otherwise = \case
        x :<| xs -> x :<|| Seq.insertAt (i - 1) y xs
        Empty    -> y :<|| Seq.empty
{-# INLINE insertSeqAt #-}

-- | \( O(1) \). Add an element to the right end of a non-empty sequence.
-- Mnemonic: a triangle with the single element at the pointy end.
(|>) :: NESeq a -> a -> NESeq a
(x :<|| xs) |> y = x :<|| (xs Seq.|> y)
{-# INLINE (|>) #-}

-- | \( O(\log(\min(n_1,n_2))) \). Concatenate a non-empty sequence with
-- a potentially empty sequence ('Seq'), to produce a guaranteed non-empty
-- sequence.  Mnemonic: like '><', but a pipe for the guarunteed non-empty
-- side.
(><|) :: Seq a -> NESeq a -> NESeq a
xs ><| ys = withNonEmpty ys (>< ys) xs
{-# INLINE (><|) #-}

infixl 5 |>
infixr 5 ><|

-- | 'replicateA' is an 'Applicative' version of 'replicate', and makes \(
-- O(\log n) \) calls to 'liftA2' and 'pure'.  Is only defined when @n@ is
-- positive.
--
-- > replicateA n x = sequenceA (replicate n x)
--
-- Is a more restrictive version of 'replicateA1'.  'replicateA1' should be
-- preferred whenever possible.
replicateA :: Applicative f => Int -> f a -> f (NESeq a)
replicateA n x
    | n < 1     = error "NESeq.replicateA: must take a positive integer argument"
    | otherwise = liftA2 (:<||) x (Seq.replicateA (n - 1) x)
{-# INLINE replicateA #-}

-- | 'replicateA' is an 'Apply' version of 'replicate', and makes \( O(\log
-- n) \) calls to '<.>'.  Is only defined when @n@ is positive.
--
-- > replicateA1 n x = sequence1 (replicate n x)
replicateA1 :: Apply f => Int -> f a -> f (NESeq a)
replicateA1 n x
    | n < 1     = error "NESeq.replicateA1: must take a positive integer argument"
    | otherwise = case runMaybeApply (Seq.replicateA (n - 1) (MaybeApply (Left x))) of
        Left  xs -> (:<||)    <$> x <.> xs
        Right xs -> (:<|| xs) <$> x
{-# INLINE replicateA1 #-}

-- | An alias of 'replicateA'.
replicateM :: Applicative m => Int -> m a -> m (NESeq a)
replicateM = replicateA
{-# INLINE replicateM #-}

-- | /O(/log/ k)/. @'cycleTaking' k xs@ forms a sequence of length @k@ by
-- repeatedly concatenating @xs@ with itself. Is only defined when @k@ is
-- positive.
--
-- prop> cycleTaking k = fromList . fromJust . nonEmpty . take k . cycle . toList

-- If you wish to concatenate a non-empty sequence @xs@ with itself precisely
-- @k@ times, you can use @cycleTaking (k * length xs)@ or just
-- @replicate k () *> xs@.
cycleTaking :: Int -> NESeq a -> NESeq a
cycleTaking n xs0@(x :<|| xs)
    | n < 1             = error "NESeq.cycleTaking: must take a positive integer argument"
    | n < Seq.length xs = x :<|| Seq.take (n - 1) xs
    | otherwise         = xs0 |>< Seq.cycleTaking (n - length xs0) (toSeq xs0)
{-# INLINE cycleTaking #-}

-- | \( O(n) \).  Constructs a sequence by repeated application of
-- a function to a seed value.  Is only defined if given a positive value.
--
-- > iterateN n f x = fromList (fromJust (nonEmpty ((Prelude.take n (Prelude.iterate f x)))))
iterateN :: Int -> (a -> a) -> a -> NESeq a
iterateN n f x
    | n < 1     = error "NESeq.iterateN: must take a positive integer argument"
    | otherwise = x :<|| Seq.iterateN (n - 1) f (f x)
{-# INLINE iterateN #-}

-- | Builds a sequence from a seed value.  Takes time linear in the
-- number of generated elements.  /WARNING:/ If the number of generated
-- elements is infinite, this method will not terminate.
unfoldr :: (b -> (a, Maybe b)) -> b -> NESeq a
unfoldr f = go
  where
    go x0 = y :<|| maybe Seq.empty (toSeq . go) x1
      where
        (y, x1) = f x0
{-# INLINE unfoldr #-}

-- | @'unfoldl' f x@ is equivalent to @'reverse' ('unfoldr' ('fmap' swap . f) x)@.
unfoldl :: (b -> (Maybe b, a)) -> b -> NESeq a
unfoldl f = go
  where
    go x0 = maybe Seq.empty (toSeq . go) x1 :||> y
      where
        (x1, y) = f x0
{-# INLINE unfoldl #-}

-- | /O(1)/. Retrieve the left-most item in a non-empty sequence.  Note
-- that this function is total.
head :: NESeq a -> a
head (x :<|| _) = x
{-# INLINE head #-}

-- | /O(1)/. Delete the left-most item in a non-empty sequence.  Returns
-- a potentially empty sequence ('Seq') in the case that the original
-- 'NESeq' contained only a single element.  Note that this function is
-- total.
tail :: NESeq a -> Seq a
tail (_ :<|| xs) = xs
{-# INLINE tail #-}

-- | /O(1)/. Retrieve the right-most item in a non-empty sequence.  Note
-- that this function is total.
last :: NESeq a -> a
last (_ :||> x) = x
{-# INLINE last #-}

-- | /O(1)/. Delete the right-most item in a non-empty sequence.  Returns
-- a potentially empty sequence ('Seq') in the case that the original
-- 'NESeq' contained only a single element.  Note that this function is
-- total.
init :: NESeq a -> Seq a
init (xs :||> _) = xs
{-# INLINE init #-}


-- | 'scanl' is similar to 'foldl', but returns a sequence of reduced
-- values from the left:
--
-- > scanl f z (fromList [x1, x2, ...]) = fromList [z, z `f` x1, (z `f` x1) `f` x2, ...]
scanl :: (a -> b -> a) -> a -> NESeq b -> NESeq a
scanl f y0 (x :<|| xs) = y0 :<|| Seq.scanl f (f y0 x) xs
{-# INLINE scanl #-}

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
--
-- > scanl1 f (fromList [x1, x2, ...]) = fromList [x1, x1 `f` x2, ...]
scanl1 :: (a -> a -> a) -> NESeq a -> NESeq a
scanl1 f (x :<|| xs) = withNonEmpty (singleton x) (scanl f x) xs
{-# INLINE scanl1 #-}

-- | 'scanr' is the right-to-left dual of 'scanl'.
scanr :: (a -> b -> b) -> b -> NESeq a -> NESeq b
scanr f y0 (xs :||> x) = Seq.scanr f (f x y0) xs :||> y0
{-# INLINE scanr #-}

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (a -> a -> a) -> NESeq a -> NESeq a
scanr1 f (xs :||> x) = withNonEmpty (singleton x) (scanr f x) xs
{-# INLINE scanr1 #-}

-- | \( O(n) \).  Returns a sequence of all non-empty prefixes of this
-- sequence, shortest first.  For example,
--
-- > tails (fromList (1:|[2,3])) = fromList (fromList (1:|[]) :| [fromList (1:|[2]), fromList (1:|[2,3]))
--
-- Evaluating the \( i \)th prefix takes \( O(\log(\min(i, n-i))) \), but evaluating
-- every prefix in the sequence takes \( O(n) \) due to sharing.

-- TODO: is this true?
inits :: NESeq a -> NESeq (NESeq a)
inits xs@(ys :||> _) = withNonEmpty (singleton xs) ((|> xs) . inits) ys
{-# INLINABLE inits #-}

-- | \(O \Bigl(\bigl(\frac{n}{c}\bigr) \log c\Bigr)\). @chunksOf c xs@ splits @xs@ into chunks of size @c>0@.
-- If @c@ does not divide the length of @xs@ evenly, then the last element
-- of the result will be short.  Is only defined if @c@ is a positive
-- number.
--
-- Side note: the given performance bound is missing some messy terms that only
-- really affect edge cases. Performance degrades smoothly from \( O(1) \) (for
-- \( c = n \)) to \( O(n) \) (for \( c = 1 \)). The true bound is more like
-- \( O \Bigl( \bigl(\frac{n}{c} - 1\bigr) (\log (c + 1)) + 1 \Bigr) \)

-- TODO: is this true?
chunksOf :: Int -> NESeq a -> NESeq (NESeq a)
chunksOf n = go
  where
    go xs = case splitAt n xs of
      This  ys    -> singleton ys
      That     _  -> e
      These ys zs -> ys <| go zs
    e = error "chunksOf: A non-empty sequence can only be broken up into positively-sized chunks."
{-# INLINABLE chunksOf #-}

-- | \( O(i) \) where \( i \) is the prefix length. 'takeWhileL', applied
-- to a predicate @p@ and a sequence @xs@, returns the longest prefix
-- (possibly empty) of @xs@ of elements that satisfy @p@.
--
-- Returns a possibly empty sequence ('Seq') in the case that the predicate
-- fails on the first item.
takeWhileL :: (a -> Bool) -> NESeq a -> Seq a
takeWhileL p (x :<|| xs)
    | p x       = x Seq.<| Seq.takeWhileL p xs
    | otherwise = Seq.empty
{-# INLINE takeWhileL #-}

-- | \( O(i) \) where \( i \) is the suffix length.  'takeWhileR', applied
-- to a predicate @p@ and a sequence @xs@, returns the longest suffix
-- (possibly empty) of @xs@ of elements that satisfy @p@.
--
-- Returns a possibly empty sequence ('Seq') in the case that the predicate
-- fails on the first item.
--
-- @'takeWhileR' p xs@ is equivalent to @'reverse' ('takeWhileL' p ('reverse' xs))@.
takeWhileR :: (a -> Bool) -> NESeq a -> Seq a
takeWhileR p (xs :||> x)
    | p x       = Seq.takeWhileR p xs Seq.|> x
    | otherwise = Seq.empty
{-# INLINE takeWhileR #-}

-- | \( O(i) \) where \( i \) is the prefix length.  @'dropWhileL' p xs@ returns
-- the suffix remaining after @'takeWhileL' p xs@.
--
-- Returns a possibly empty sequence ('Seq') in the case that the predicate
-- passes for all items.
dropWhileL :: (a -> Bool) -> NESeq a -> Seq a
dropWhileL p xs0@(x :<|| xs)
    | p x       = Seq.dropWhileL p xs
    | otherwise = toSeq xs0
{-# INLINE dropWhileL #-}

-- | \( O(i) \) where \( i \) is the suffix length.  @'dropWhileR' p xs@ returns
-- the prefix remaining after @'takeWhileR' p xs@.
--
-- Returns a possibly empty sequence ('Seq') in the case that the predicate
-- passes for all items.
--
-- @'dropWhileR' p xs@ is equivalent to @'reverse' ('dropWhileL' p ('reverse' xs))@.
dropWhileR :: (a -> Bool) -> NESeq a -> Seq a
dropWhileR p xs0@(xs :||> x)
    | p x       = Seq.dropWhileR p xs
    | otherwise = toSeq xs0
{-# INLINE dropWhileR #-}

-- | \( O(i) \) where \( i \) is the prefix length.  'spanl', applied to
-- a predicate @p@ and a sequence @xs@, returns a 'These' based on the
-- point where the predicate fails:
--
-- *   @'This' ys@ means that the predicate was true for all items, and
--     @ys@ is the entire original sequence.
-- *   @'That' zs@ means that the predicate failed on the first item, and
--     @zs@ is the entire original sequence.
-- *   @'These' ys zs@ gives @ys@ (the prefix of elements that satisfy the
--     predicae) and @zs@ (the remainder of the sequence)
spanl :: (a -> Bool) -> NESeq a -> These (NESeq a) (NESeq a)
spanl p xs0@(x :<|| xs)
    | p x       = case (nonEmptySeq ys, nonEmptySeq zs) of
        (Nothing , Nothing ) -> This  (singleton x)
        (Just _  , Nothing ) -> This  xs0
        (Nothing , Just zs') -> These (singleton x) zs'
        (Just ys', Just zs') -> These (x <| ys')    zs'
    | otherwise = That xs0
  where
    (ys, zs) = Seq.spanl p xs
{-# INLINABLE spanl #-}

-- | \( O(i) \) where \( i \) is the suffix length.  'spanr', applied to
-- a predicate @p@ and a sequence @xs@, returns a 'These' based on the
-- point where the predicate fails:
--
-- *   @'This' ys@ means that the predicate was true for all items, and
--     @ys@ is the entire original sequence.
-- *   @'That' zs@ means that the predicate failed on the first item, and
--     @zs@ is the entire original sequence.
-- *   @'These' ys zs@ gives @ys@ (the suffix of elements that satisfy the
--     predicae) and @zs@ (the remainder of the sequence, before the suffix)
spanr :: (a -> Bool) -> NESeq a -> These (NESeq a) (NESeq a)
spanr p xs0@(xs :||> x)
    | p x       = case (nonEmptySeq ys, nonEmptySeq zs) of
        (Nothing , Nothing ) -> This  (singleton x)
        (Just _  , Nothing ) -> This  xs0
        (Nothing , Just zs') -> These (singleton x) zs'
        (Just ys', Just zs') -> These (ys' |> x   ) zs'
    | otherwise = That xs0
  where
    (ys, zs) = Seq.spanr p xs
{-# INLINABLE spanr #-}

-- | \( O(i) \) where \( i \) is the breakpoint index.
--
-- @'breakl' p@ is @'spanl' (not . p)@.
breakl :: (a -> Bool) -> NESeq a -> These (NESeq a) (NESeq a)
breakl p = spanl (not . p)
{-# INLINE breakl #-}

-- | \( O(i) \) where \( i \) is the breakpoint index.
--
-- @'breakr' p@ is @'spanr' (not . p)@.
breakr :: (a -> Bool) -> NESeq a -> These (NESeq a) (NESeq a)
breakr p = spanr (not . p)
{-# INLINE breakr #-}

-- | \( O(n) \).  The 'partition' function takes a predicate @p@ and a
-- sequence @xs@ and returns sequences of those elements which do and
-- do not satisfy the predicate, as a 'These':
--
-- *   @'This' ys@ means that the predicate was true for all items, and
--     @ys@ is the entire original sequence.
-- *   @'That' zs@ means that the predicate failed on the first item, and
--     @zs@ is the entire original sequence.
-- *   @'These' ys zs@ gives @ys@ (the sequence of elements for which the
--     predicate was true) and @zs@ (the sequence of elements for which the
--     predicate was false).
partition :: (a -> Bool) -> NESeq a -> These (NESeq a) (NESeq a)
partition p xs0@(x :<|| xs) = case (nonEmptySeq ys, nonEmptySeq zs) of
    (Nothing , Nothing )
      | p x       -> This  (singleton x)
      | otherwise -> That                (singleton x)
    (Just ys', Nothing )
      | p x       -> This  xs0
      | otherwise -> These ys'           (singleton x)
    (Nothing, Just zs' )
      | p x       -> These (singleton x) zs'
      | otherwise -> That                xs0
    (Just ys', Just zs')
      | p x       -> These (x <| ys')    zs'
      | otherwise -> These ys'           (x <| zs')
  where
    (ys, zs) = Seq.partition p xs
{-# INLINABLE partition #-}

-- | \( O(n) \).  The 'filter' function takes a predicate @p@ and a sequence
-- @xs@ and returns a sequence of those elements which satisfy the
-- predicate.
--
-- Returns a potentially empty sequence ('Seq') in the case that the
-- predicate fails for all items in the sequence.
filter :: (a -> Bool) -> NESeq a -> Seq a
filter p (x :<|| xs)
    | p x       = x Seq.<| Seq.filter p xs
    | otherwise = Seq.filter p xs
{-# INLINE filter #-}

-- | \( O(n \log n) \).  'sort' sorts the specified 'NESeq' by the natural
-- ordering of its elements.  The sort is stable.  If stability is not
-- required, 'unstableSort' can be slightly faster.
sort :: Ord a => NESeq a -> NESeq a
sort = sortBy compare
{-# INLINE sort #-}

-- | \( O(n \log n) \).  'sortBy' sorts the specified 'NESeq' according to
-- the specified comparator.  The sort is stable.  If stability is not
-- required, 'unstableSortBy' can be slightly faster.

-- TODO: benchmark against just unsafe unwrapping and wrapping
sortBy :: (a -> a -> Ordering) -> NESeq a -> NESeq a
sortBy c (x :<|| xs) = withNonEmpty (singleton x) (insertBy c x)
                     . Seq.sortBy c
                     $ xs
{-# INLINE sortBy #-}

-- | \( O(n \log n) \). 'sortOn' sorts the specified 'NESeq' by comparing
-- the results of a key function applied to each element. @'sortOn' f@ is
-- equivalent to @'sortBy' ('compare' ``Data.Function.on`` f)@, but has the
-- performance advantage of only evaluating @f@ once for each element in
-- the input list. This is called the decorate-sort-undecorate paradigm, or
-- Schwartzian transform.
--
-- An example of using 'sortOn' might be to sort a 'NESeq' of strings
-- according to their length:
--
-- > sortOn length (fromList ("alligator" :| ["monkey", "zebra"])) == fromList ("zebra" :| ["monkey", "alligator"])
--
-- If, instead, 'sortBy' had been used, 'length' would be evaluated on
-- every comparison, giving \( O(n \log n) \) evaluations, rather than
-- \( O(n) \).
--
-- If @f@ is very cheap (for example a record selector, or 'fst'),
-- @'sortBy' ('compare' ``Data.Function.on`` f)@ will be faster than
-- @'sortOn' f@.

-- TODO: benchmark against just unsafe unwrapping and wrapping
sortOn :: Ord b => (a -> b) -> NESeq a -> NESeq a
sortOn f (x :<|| xs) = withNonEmpty (singleton x) (insertOn f x)
                     . sortOnSeq f
                     $ xs
{-# INLINE sortOn #-}

-- | \( O(n \log n) \).  'unstableSort' sorts the specified 'NESeq' by the
-- natural ordering of its elements, but the sort is not stable.  This
-- algorithm is frequently faster and uses less memory than 'sort'.
unstableSort :: Ord a => NESeq a -> NESeq a
unstableSort = unstableSortBy compare
{-# INLINE unstableSort #-}

-- | \( O(n \log n) \).  A generalization of 'unstableSort',
-- 'unstableSortBy' takes an arbitrary comparator and sorts the specified
-- sequence.  The sort is not stable.  This algorithm is frequently faster
-- and uses less memory than 'sortBy'.

-- TODO: figure out how to make it match 'Data.Sequence.unstableSortBy'
-- without unsafe wrapping/unwrapping
unstableSortBy :: (a -> a -> Ordering) -> NESeq a -> NESeq a
unstableSortBy c = unsafeFromSeq . Seq.unstableSortBy c . toSeq
-- unstableSortBy c (x :<|| xs) = withNonEmpty (singleton x) (insertBy c x)
--                      . Seq.unstableSortBy c
--                      $ xs
{-# INLINE unstableSortBy #-}

-- | \( O(n \log n) \). 'unstableSortOn' sorts the specified 'NESeq' by
-- comparing the results of a key function applied to each element.
-- @'unstableSortOn' f@ is equivalent to @'unstableSortBy' ('compare' ``Data.Function.on`` f)@,
-- but has the performance advantage of only evaluating @f@ once for each
-- element in the input list. This is called the
-- decorate-sort-undecorate paradigm, or Schwartzian transform.
--
-- An example of using 'unstableSortOn' might be to sort a 'NESeq' of strings
-- according to their length.
--
-- > unstableSortOn length (fromList ("alligator" :| ["monkey", "zebra"])) == fromList ("zebra" :| ["monkey", "alligator]")
--
-- If, instead, 'unstableSortBy' had been used, 'length' would be evaluated on
-- every comparison, giving \( O(n \log n) \) evaluations, rather than
-- \( O(n) \).
--
-- If @f@ is very cheap (for example a record selector, or 'fst'),
-- @'unstableSortBy' ('compare' ``Data.Function.on`` f)@ will be faster than
-- @'unstableSortOn' f@.

-- TODO: figure out how to make it match 'Data.Sequence.unstableSortBy'
-- without unsafe wrapping/unwrapping
unstableSortOn :: Ord b => (a -> b) -> NESeq a -> NESeq a
unstableSortOn f = unsafeFromSeq . unstableSortOnSeq f . toSeq
-- unstableSortOn f (x :<|| xs) = withNonEmpty (singleton x) (insertOn f x)
--                              . Seq.unstableSortOn f
--                              $ xs
{-# INLINE unstableSortOn #-}

insertBy :: (a -> a -> Ordering) -> a -> NESeq a -> NESeq a
insertBy c x xs = case spanl ltx xs of
    This  ys    -> ys |> x
    That     zs -> x <| zs
    These ys zs -> ys >< (x <| zs)
  where
    ltx y = c x y == GT
{-# INLINABLE insertBy #-}

insertOn :: Ord b => (a -> b) -> a -> NESeq a -> NESeq a
insertOn f x xs = case spanl ltx xs of
    This  ys    -> ys |> x
    That     zs -> x <| zs
    These ys zs -> ys >< (x <| zs)
  where
    fx = f x
    ltx y = fx > f y
{-# INLINABLE insertOn #-}

-- | \( O(\log(\min(i,n-i))) \). The element at the specified position,
-- counting from 0. If the specified position is negative or at
-- least the length of the sequence, 'lookup' returns 'Nothing'.
--
-- Unlike 'index', this can be used to retrieve an element without
-- forcing it.
lookup :: Int -> NESeq a -> Maybe a
lookup 0 (x :<|| _ ) = Just x
lookup i (_ :<|| xs) = Seq.lookup (i - 1) xs
{-# INLINE lookup #-}

-- | \( O(\log(\min(i,n-i))) \). A flipped, infix version of `lookup`.
(!?) :: NESeq a -> Int -> Maybe a
(!?) = flip lookup
{-# INLINE (!?) #-}

-- | \( O(\log(\min(i,n-i))) \). Update the element at the specified position.  If
-- the position is out of range, the original sequence is returned.  'adjust'
-- can lead to poor performance and even memory leaks, because it does not
-- force the new value before installing it in the sequence. 'adjust'' should
-- usually be preferred.
adjust :: (a -> a) -> Int -> NESeq a -> NESeq a
adjust f 0 (x :<|| xs) = f x :<|| xs
adjust f i (x :<|| xs) = x :<|| Seq.adjust f (i - 1) xs
{-# INLINE adjust #-}

-- | \( O(\log(\min(i,n-i))) \). Update the element at the specified position.
-- If the position is out of range, the original sequence is returned.
-- The new value is forced before it is installed in the sequence.
--
-- @
-- adjust' f i xs =
--  case xs !? i of
--    Nothing -> xs
--    Just x -> let !x' = f x
--              in update i x' xs
-- @
adjust' :: (a -> a) -> Int -> NESeq a -> NESeq a
adjust' f 0 (x :<|| xs) = let !y  = f x in y :<|| xs
adjust' f i (x :<|| xs) = x :<|| Seq.adjust f (i - 1) xs
{-# INLINE adjust' #-}

-- | \( O(\log(\min(i,n-i))) \). Replace the element at the specified position.
-- If the position is out of range, the original sequence is returned.
update :: Int -> a -> NESeq a -> NESeq a
update 0 y (_ :<|| xs) = y :<|| xs
update i y (x :<|| xs) = x :<|| Seq.update (i - 1) y xs
{-# INLINE update #-}

-- | \( O(\log(\min(i,n-i))) \). The first @i@ elements of a sequence.
-- If @i@ is negative, @'take' i s@ yields the empty sequence.
-- If the sequence contains fewer than @i@ elements, the whole sequence
-- is returned.
take :: Int -> NESeq a -> Seq a
take i (x :<|| xs)
    | i <= 0    = Seq.empty
    | otherwise = x Seq.<| Seq.take (i - 1) xs
{-# INLINE take #-}

-- | \( O(\log(\min(i,n-i))) \). Elements of a sequence after the first @i@.
-- If @i@ is negative, @'drop' i s@ yields the whole sequence.
-- If the sequence contains fewer than @i@ elements, the empty sequence
-- is returned.
drop :: Int -> NESeq a -> Seq a
drop i xs0@(_ :<|| xs)
    | i <= 0    = toSeq xs0
    | otherwise = Seq.drop (i - 1) xs
{-# INLINE drop #-}

-- | \( O(\log(\min(i,n-i))) \). @'insertAt' i x xs@ inserts @x@ into @xs@
-- at the index @i@, shifting the rest of the sequence over.
--
-- @
-- insertAt 2 x (fromList (a:|[b,c,d])) = fromList (a:|[b,x,c,d])
-- insertAt 4 x (fromList (a:|[b,c,d])) = insertAt 10 x (fromList (a:|[b,c,d]))
--                                      = fromList (a:|[b,c,d,x])
-- @
--
-- prop> insertAt i x xs = take i xs >< singleton x >< drop i xs
insertAt :: Int -> a -> NESeq a -> NESeq a
insertAt i y xs0@(x :<|| xs)
    | i <= 0    = y <| xs0
    | otherwise = x :<|| Seq.insertAt (i - 1) y xs
{-# INLINE insertAt #-}

-- | \( O(\log(\min(i,n-i))) \). Delete the element of a sequence at a given
-- index. Return the original sequence if the index is out of range.
--
-- @
-- deleteAt 2 (a:|[b,c,d]) = a:|[b,d]
-- deleteAt 4 (a:|[b,c,d]) = deleteAt (-1) (a:|[b,c,d]) = a:|[b,c,d]
-- @
deleteAt :: Int -> NESeq a -> Seq a
deleteAt i xs0@(x :<|| xs) = case compare i 0 of
    LT -> toSeq xs0
    EQ -> xs
    GT -> x Seq.<| Seq.deleteAt (i - 1) xs
{-# INLINE deleteAt #-}

-- | \( O(\log(\min(i,n-i))) \). Split a sequence at a given position.
--
-- *   @'This' ys@ means that the given position was longer than the length
--     of the list, and @ys@ is the entire original system.
-- *   @'That' zs@ means that the given position was zero or smaller, and
--     so @zs@ is the entire original sequence.
-- *   @'These' ys zs@ gives @ys@ (the sequence of elements before the
--     given position, @take n xs@) and @zs@ (the sequence of elements
--     after the given position, @drop n xs@).
splitAt :: Int -> NESeq a -> These (NESeq a) (NESeq a)
splitAt n xs0@(x :<|| xs)
    | n <= 0    = That xs0
    | otherwise = case (nonEmptySeq ys, nonEmptySeq zs) of
        (Nothing , Nothing ) -> This  (singleton x)
        (Just _  , Nothing ) -> This  xs0
        (Nothing , Just zs') -> These (singleton x) zs'
        (Just ys', Just zs') -> These (x <| ys')    zs'
  where
    (ys, zs) = Seq.splitAt (n - 1) xs
{-# INLINABLE splitAt #-}

-- | 'elemIndexL' finds the leftmost index of the specified element,
-- if it is present, and otherwise 'Nothing'.
elemIndexL :: Eq a => a -> NESeq a -> Maybe Int
elemIndexL x = findIndexL (== x)
{-# INLINE elemIndexL #-}

-- | 'elemIndexR' finds the rightmost index of the specified element,
-- if it is present, and otherwise 'Nothing'.
elemIndexR :: Eq a => a -> NESeq a -> Maybe Int
elemIndexR x = findIndexR (== x)
{-# INLINE elemIndexR #-}

-- | 'elemIndicesL' finds the indices of the specified element, from
-- left to right (i.e. in ascending order).
elemIndicesL :: Eq a => a -> NESeq a -> [Int]
elemIndicesL x = findIndicesL (== x)
{-# INLINE elemIndicesL #-}

-- | 'elemIndicesR' finds the indices of the specified element, from
-- right to left (i.e. in descending order).
elemIndicesR :: Eq a => a -> NESeq a -> [Int]
elemIndicesR x = findIndicesR (== x)
{-# INLINE elemIndicesR #-}

-- | @'findIndexL' p xs@ finds the index of the leftmost element that
-- satisfies @p@, if any exist.
findIndexL :: (a -> Bool) -> NESeq a -> Maybe Int
findIndexL p (x :<|| xs) = here_ <|> there_
  where
    here_  = 0 <$ guard (p x)
    there_ = (+ 1) <$> Seq.findIndexL p xs
{-# INLINE findIndexL #-}

-- | @'findIndexR' p xs@ finds the index of the rightmost element that
-- satisfies @p@, if any exist.
findIndexR :: (a -> Bool) -> NESeq a -> Maybe Int
findIndexR p (xs :||> x) = here_ <|> there_
  where
    here_  = Seq.length xs <$ guard (p x)
    there_ = Seq.findIndexR p xs
{-# INLINE findIndexR #-}

-- | @'findIndicesL' p@ finds all indices of elements that satisfy @p@,
-- in ascending order.

-- TODO: use build
findIndicesL :: (a -> Bool) -> NESeq a -> [Int]
findIndicesL p (x :<|| xs)
    | p x       = 0 : ixs
    | otherwise = ixs
  where
    ixs = (+ 1) <$> Seq.findIndicesL p xs
{-# INLINE findIndicesL #-}

-- | @'findIndicesR' p@ finds all indices of elements that satisfy @p@,
-- in descending order.

-- TODO: use build
findIndicesR :: (a -> Bool) -> NESeq a -> [Int]
findIndicesR p (xs :||> x)
    | p x       = Seq.length xs : ixs
    | otherwise = ixs
  where
    ixs = Seq.findIndicesR p xs
{-# INLINE findIndicesR #-}

-- | 'foldlWithIndex' is a version of 'foldl' that also provides access
-- to the index of each element.
foldlWithIndex :: (b -> Int -> a -> b) -> b -> NESeq a -> b
foldlWithIndex f z (xs :||> x) = (\z' -> f z' (Seq.length xs) x) . Seq.foldlWithIndex f z $ xs
{-# INLINE foldlWithIndex #-}

-- | 'foldrWithIndex' is a version of 'foldr' that also provides access
-- to the index of each element.
foldrWithIndex :: (Int -> a -> b -> b) -> b -> NESeq a -> b
foldrWithIndex f z (x :<|| xs) = f 0 x . Seq.foldrWithIndex (f . (+ 1)) z $ xs
{-# INLINE foldrWithIndex #-}

-- | A generalization of 'fmap', 'mapWithIndex' takes a mapping
-- function that also depends on the element's index, and applies it to every
-- element in the sequence.
mapWithIndex :: (Int -> a -> b) -> NESeq a -> NESeq b
mapWithIndex f (x :<|| xs) = f 0 x :<|| Seq.mapWithIndex (f . (+ 1)) xs
{-# NOINLINE [1] mapWithIndex #-}
{-# RULES
"mapWithIndex/mapWithIndex" forall f g xs . mapWithIndex f (mapWithIndex g xs) =
  mapWithIndex (\k a -> f k (g k a)) xs
"mapWithIndex/map" forall f g xs . mapWithIndex f (map g xs) =
  mapWithIndex (\k a -> f k (g a)) xs
"map/mapWithIndex" forall f g xs . map f (mapWithIndex g xs) =
  mapWithIndex (\k a -> f (g k a)) xs
 #-}

-- | 'traverseWithIndex' is a version of 'traverse' that also offers
-- access to the index of each element.
--
-- Is a more restrictive version of 'traverseWithIndex1';
-- 'traverseWithIndex1' should be used whenever possible.
traverseWithIndex :: Applicative f => (Int -> a -> f b) -> NESeq a -> f (NESeq b)
traverseWithIndex f (x :<|| xs) = (:<||) <$> f 0 x <*> Seq.traverseWithIndex (f . (+ 1)) xs
{-# NOINLINE [1] traverseWithIndex #-}
{-# RULES
"travWithIndex/mapWithIndex" forall f g xs . traverseWithIndex f (mapWithIndex g xs) =
  traverseWithIndex (\k a -> f k (g k a)) xs
"travWithIndex/map" forall f g xs . traverseWithIndex f (map g xs) =
  traverseWithIndex (\k a -> f k (g a)) xs
 #-}

-- | \( O(n) \). The reverse of a sequence.
reverse :: NESeq a -> NESeq a
reverse (x :<|| xs) = Seq.reverse xs :||> x
{-# NOINLINE [1] reverse #-}

-- | \( O(n) \). Reverse a sequence while mapping over it. This is not
-- currently exported, but is used in rewrite rules.
mapReverse :: (a -> b) -> NESeq a -> NESeq b
mapReverse f (x :<|| xs) = fmap f (Seq.reverse xs) :||> f x

{-# RULES
"map/reverse" forall f xs . map f (reverse xs) = mapReverse f xs
"reverse/map" forall f xs . reverse (map f xs) = mapReverse f xs
 #-}

-- | \( O(n) \). Intersperse an element between the elements of a sequence.
--
-- @
-- intersperse a empty = empty
-- intersperse a (singleton x) = singleton x
-- intersperse a (fromList [x,y]) = fromList [x,a,y]
-- intersperse a (fromList [x,y,z]) = fromList [x,a,y,a,z]
-- @
intersperse :: a -> NESeq a -> NESeq a
intersperse z nes@(x :<|| xs) = case xs of
  _ Seq.:<| _ -> x :<|| (z Seq.<| Seq.intersperse z xs)
  Seq.Empty -> nes
{-# INLINE intersperse #-}

-- | \( O(\min(n_1,n_2,n_3)) \).  'zip3' takes three sequences and returns a
-- sequence of triples, analogous to 'zip'.
zip3 :: NESeq a -> NESeq b -> NESeq c -> NESeq (a, b, c)
zip3 (x :<|| xs) (y :<|| ys) (z :<|| zs) = (x, y, z) :<|| Seq.zip3 xs ys zs
{-# INLINE zip3 #-}

-- | \( O(\min(n_1,n_2,n_3)) \).  'zipWith3' takes a function which combines
-- three elements, as well as three sequences and returns a sequence of
-- their point-wise combinations, analogous to 'zipWith'.
zipWith3 :: (a -> b -> c -> d) -> NESeq a -> NESeq b -> NESeq c -> NESeq d
zipWith3 f (x :<|| xs) (y :<|| ys) (z :<|| zs) = f x y z :<|| Seq.zipWith3 f xs ys zs
{-# INLINE zipWith3 #-}

-- | \( O(\min(n_1,n_2,n_3,n_4)) \).  'zip4' takes four sequences and returns a
-- sequence of quadruples, analogous to 'zip'.
zip4 :: NESeq a -> NESeq b -> NESeq c -> NESeq d -> NESeq (a, b, c, d)
zip4 (x :<|| xs) (y :<|| ys) (z :<|| zs) (r :<|| rs) = (x, y, z, r) :<|| Seq.zip4 xs ys zs rs
{-# INLINE zip4 #-}

-- | \( O(\min(n_1,n_2,n_3,n_4)) \).  'zipWith4' takes a function which combines
-- four elements, as well as four sequences and returns a sequence of
-- their point-wise combinations, analogous to 'zipWith'.
zipWith4 :: (a -> b -> c -> d -> e) -> NESeq a -> NESeq b -> NESeq c -> NESeq d -> NESeq e
zipWith4 f (x :<|| xs) (y :<|| ys) (z :<|| zs) (r :<|| rs) = f x y z r :<|| Seq.zipWith4 f xs ys zs rs
{-# INLINE zipWith4 #-}

-- | \( O(n) \). Unzip a sequence using a function to divide elements.
--
-- @ unzipWith f xs == 'unzip' ('fmap' f xs) @
--
-- Efficiency note:
--
-- @unzipWith@ produces its two results in lockstep. If you calculate
-- @ unzipWith f xs @ and fully force /either/ of the results, then the
-- entire structure of the /other/ one will be built as well. This
-- behavior allows the garbage collector to collect each calculated
-- pair component as soon as it dies, without having to wait for its mate
-- to die. If you do not need this behavior, you may be better off simply
-- calculating the sequence of pairs and using 'fmap' to extract each
-- component sequence.
unzipWith :: (a -> (b, c)) -> NESeq a -> (NESeq b, NESeq c)
unzipWith f (x :<|| xs) = bimap (y :<||) (z :<||) . unzipWithSeq f $ xs
  where
    ~(y, z) = f x
{-# NOINLINE [1] unzipWith #-}

{-# RULES
"unzipWith/map" forall f g xs. unzipWith f (map g xs) =
                                     unzipWith (f . g) xs
 #-}
