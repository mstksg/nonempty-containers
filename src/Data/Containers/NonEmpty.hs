{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Data.Containers.NonEmpty
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- = Non-Empty Typeclass
--
-- Provides the typeclass 'HasNonEmpty', which abstracts over different
-- types which have a "non-empty" variant.
--
-- Used to convert between and in between possibly-empty and non-empty
-- types.  Instances are provided for all modules in this package, as well
-- as for 'NonEmpty' in /base/ and 'NonEmptyVector'.
module Data.Containers.NonEmpty (
  HasNonEmpty (..),
  pattern IsNonEmpty,
  pattern IsEmpty,
  overNonEmpty,
  onNonEmpty,
) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntMap.NonEmpty (NEIntMap)
import qualified Data.IntMap.NonEmpty as NEIM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.IntSet.NonEmpty (NEIntSet)
import qualified Data.IntSet.NonEmpty as NEIS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEM
import Data.Maybe
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq
import Data.Set (Set)
import qualified Data.Set as S
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NEV

-- | If @s@ is an instance of @HasNonEmpty@, it means that there is
-- a corresponding "non-empty" version of @s@, @'NE' s@.
--
-- In order for things to be well-behaved, we expect that 'nonEmpty' and
-- @maybe 'empty' 'fromNonEmpty'@ should form an isomorphism (or that
-- @'withNonEmpty' 'empty' 'fromNonEmpty' == id@.  In addition,
-- the following properties should hold for most exectations:
--
-- *    @(x == empty) ==> isEmpty x@
-- *    @(x == empty) ==> isNothing (nonEmpty x)@
-- *    @isEmpty x    ==> isNothing (nonEmpty x)@
-- *    @unsafeToNonEmpty x == fromJust (nonEmpty x)@
-- *    Usually, @not (isEmpty x) ==> isJust (nonEmpty x)@, but this isn't
--      necessary.
class HasNonEmpty s where
  {-# MINIMAL (nonEmpty | withNonEmpty), fromNonEmpty, empty #-}

  -- | @'NE' s@ is the "non-empty" version of @s@.
  type NE s = t | t -> s

  -- | "Smart constructor" for @'NE' s@ given a (potentailly empty) @s@.
  -- Will return 'Nothing' if the @s@ was empty, and @'Just' n@ if the
  -- @s@ was not empty, with @n :: 'NE' s@.
  --
  -- Should form an isomorphism with @'maybe' 'empty' 'fromNonEmpty'@.
  nonEmpty :: s -> Maybe (NE s)
  nonEmpty = withNonEmpty Nothing Just

  -- | Convert a @'NE' s@ (non-empty @s@) back into an @s@, "obscuring"
  -- its non-emptiness from its type.
  fromNonEmpty :: NE s -> s

  -- | Continuation-based version of 'nonEmpty', which can be more
  -- efficient in certain situations.
  --
  -- @'withNonEmpty' 'empty' 'fromNonEmpty'@ should be @id@.
  withNonEmpty :: r -> (NE s -> r) -> s -> r
  withNonEmpty def f = maybe def f . nonEmpty

  -- | An empty @s@.
  empty :: s

  -- | Check if an @s@ is empty.
  isEmpty :: s -> Bool
  isEmpty = isNothing . nonEmpty

  -- | Unsafely coerce an @s@ into an @'NE' s@ (non-empty @s@).  Is
  -- undefined (throws a runtime exception when evaluation is attempted)
  -- when the @s@ is empty.
  unsafeToNonEmpty :: s -> NE s
  unsafeToNonEmpty = fromMaybe e . nonEmpty
    where
      e = errorWithoutStackTrace "unsafeToNonEmpty: empty input provided"

-- | Useful function for mapping over the "non-empty" representation of
-- a type.
--
-- @since 0.3.3.0
overNonEmpty :: (HasNonEmpty s, HasNonEmpty t) => (NE s -> NE t) -> s -> t
overNonEmpty f = withNonEmpty empty (fromNonEmpty . f)

-- | Useful function for applying a function on the "non-empty"
-- representation of a type.
--
-- If you want a continuation taking @'NE' s -> 'Maybe r'@, you can
-- use @'withNonEmpty' 'Nothing'@.
--
-- @since 0.3.3.0
onNonEmpty :: HasNonEmpty s => (NE s -> r) -> s -> Maybe r
onNonEmpty f = withNonEmpty Nothing (Just . f)

instance HasNonEmpty [a] where
  type NE [a] = NonEmpty a
  nonEmpty = NE.nonEmpty
  fromNonEmpty = NE.toList
  withNonEmpty def f = \case
    [] -> def
    x : xs -> f (x :| xs)
  empty = []
  isEmpty = null
  unsafeToNonEmpty = NE.fromList

instance HasNonEmpty (Map k a) where
  type NE (Map k a) = NEMap k a
  nonEmpty = NEM.nonEmptyMap
  fromNonEmpty = NEM.toMap
  withNonEmpty = NEM.withNonEmpty
  empty = M.empty
  isEmpty = M.null
  unsafeToNonEmpty = NEM.unsafeFromMap

instance HasNonEmpty (IntMap a) where
  type NE (IntMap a) = NEIntMap a
  nonEmpty = NEIM.nonEmptyMap
  fromNonEmpty = NEIM.toMap
  withNonEmpty = NEIM.withNonEmpty
  empty = IM.empty
  isEmpty = IM.null
  unsafeToNonEmpty = NEIM.unsafeFromMap

instance HasNonEmpty (Set a) where
  type NE (Set a) = NESet a
  nonEmpty = NES.nonEmptySet
  fromNonEmpty = NES.toSet
  withNonEmpty = NES.withNonEmpty
  empty = S.empty
  isEmpty = S.null
  unsafeToNonEmpty = NES.unsafeFromSet

instance HasNonEmpty IntSet where
  type NE IntSet = NEIntSet
  nonEmpty = NEIS.nonEmptySet
  fromNonEmpty = NEIS.toSet
  withNonEmpty = NEIS.withNonEmpty
  empty = IS.empty
  isEmpty = IS.null
  unsafeToNonEmpty = NEIS.unsafeFromSet

instance HasNonEmpty (Seq a) where
  type NE (Seq a) = NESeq a
  nonEmpty = NESeq.nonEmptySeq
  fromNonEmpty = NESeq.toSeq
  withNonEmpty = NESeq.withNonEmpty
  empty = Seq.empty
  isEmpty = Seq.null
  unsafeToNonEmpty = NESeq.unsafeFromSeq

instance HasNonEmpty (Vector a) where
  type NE (Vector a) = NonEmptyVector a
  nonEmpty = NEV.fromVector
  fromNonEmpty = NEV.toVector
  empty = V.empty
  isEmpty = V.null

-- | The 'IsNonEmpty' and 'IsEmpty' patterns allow you to treat a @s@ as
-- if it were either a @'IsNonEmpty' n@ (where @n@ is a non-empty version
-- of @s@, type @'NE' s@) or an 'IsEmpty'.
--
-- For example, you can pattern match on a list to get a 'NonEmpty'
-- (non-empty list):
--
-- @
-- safeHead :: [Int] -> Int
-- safeHead ('IsNonEmpty' (x :| _)) = x     -- here, the list was not empty
-- safehead 'IsEmpty'               = 0     -- here, the list was empty
-- @
--
-- Matching on @'IsNonEmpty' n@ means that the original input was /not/
-- empty, and you have a verified-non-empty @n :: 'NE' s@ to use.
--
-- Note that because of the way coverage checking works for polymorphic
-- pattern synonyms, you will unfortunatelly still get incomplete pattern
-- match warnings if you match on both 'IsNonEmpty' and 'NonEmpty', even
-- though the two are meant to provide complete coverage.  However, many
-- instances of 'HasNonEmpty' (like 'NEMap', 'NEIntMap', 'NESet',
-- 'NEIntSet') will provide their own monomorphic versions of these
-- patterns that can be verified as complete covers by GHC.
--
-- This is a bidirectional pattern, so you can use 'IsNonEmpty' to convert
-- a @'NE' s@ back into an @s@, "obscuring" its non-emptiness (see
-- 'fromNonEmpty').
pattern IsNonEmpty :: HasNonEmpty s => NE s -> s
pattern IsNonEmpty n <- (nonEmpty -> Just n)
  where
    IsNonEmpty n = fromNonEmpty n

-- | The 'IsNonEmpty' and 'IsEmpty' patterns allow you to treat a @s@ as
-- if it were either a @'IsNonEmpty' n@ (where @n@ is a non-empty version
-- of @s@, type @'NE' s@) or an 'IsEmpty'.
--
-- Matching on 'IsEmpty' means that the original item was empty.
--
-- This is a bidirectional pattern, so you can use 'IsEmpty' as an
-- expression, and it will be interpreted as 'empty'.
--
-- Note that because of the way coverage checking works for polymorphic
-- pattern synonyms, you will unfortunatelly still get incomplete pattern
-- match warnings if you match on both 'IsNonEmpty' and 'NonEmpty', even
-- though the two are meant to provide complete coverage.  However, many
-- instances of 'HasNonEmpty' (like 'NEMap', 'NEIntMap', 'NESet',
-- 'NEIntSet') will provide their own monomorphic versions of these
-- patterns that can be verified as complete covers by GHC.
--
-- See 'IsNonEmpty' for more information.
pattern IsEmpty :: HasNonEmpty s => s
pattern IsEmpty <- (isEmpty -> True)
  where
    IsEmpty = empty
