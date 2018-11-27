{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns       #-}

-- |
-- Module      : Data.IntSet.NonEmpty.Internal
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Unsafe internal-use functions used in the implementation of
-- "Data.IntSet.NonEmpty".  These functions can potentially be used to break
-- the abstraction of 'NEIntSet' and produce unsound sets, so be wary!
module Data.IntSet.NonEmpty.Internal (
    NEIntSet(..)
  , S.Key
  , nonEmptyIntSet
  , toIntSet
  , singleton
  , fromList
  , toList
  , union
  , unions
  , valid
  , insertMinIntSet
  , insertMaxIntSet
  ) where

import           Control.DeepSeq
import           Data.Data
import           Data.Function
import           Data.IntSet.Internal    (IntSet(..), Key)
import           Data.List.NonEmpty      (NonEmpty(..))
import           Data.Semigroup
import           Data.Semigroup.Foldable (Foldable1)
import           Data.Typeable           (Typeable)
import           Text.Read
import qualified Data.Foldable           as F
import qualified Data.IntSet             as S
import qualified Data.Semigroup.Foldable as F1

-- | A non-empty set of values @a@.  At least one value exists in an
-- @'NEIntSet' a@ at all times.
--
-- Functions that /take/ an 'NEIntSet' can safely operate on it with the
-- assumption that it has at least one item.
--
-- Functions that /return/ an 'NEIntSet' provide an assurance that the result
-- has at least one item.
--
-- "Data.IntSet.NonEmpty" re-exports the API of "Data.IntSet", faithfully
-- reproducing asymptotics, typeclass constraints, and semantics.
-- Functions that ensure that input and output sets are both non-empty
-- (like 'Data.IntSet.NonEmpty.insert') return 'NEIntSet', but functions that
-- might potentially return an empty map (like 'Data.IntSet.NonEmpty.delete')
-- return a 'IntSet' instead.
--
-- You can directly construct an 'NEIntSet' with the API from
-- "Data.IntSet.NonEmpty"; it's more or less the same as constructing a normal
-- 'IntSet', except you don't have access to 'Data.IntSet.empty'.  There are also
-- a few ways to construct an 'NEIntSet' from a 'IntSet':
--
-- 1.  The 'nonEmptyIntSet' smart constructor will convert a @'IntSet' a@ into
--     a @'Maybe' ('NEIntSet' a)@, returning 'Nothing' if the original 'IntSet'
--     was empty.
-- 2.  You can use the 'Data.IntSet.NonEmpty.insertIntSet' family of functions to
--     insert a value into a 'IntSet' to create a guarunteed 'NEIntSet'.
-- 3.  You can use the 'Data.IntSet.NonEmpty.IsNonEmpty' and
--     'Data.IntSet.NonEmpty.IsEmpty' patterns to "pattern match" on a 'IntSet'
--     to reveal it as either containing a 'NEIntSet' or an empty map.
-- 4.  'Data.IntSet.NonEmpty.withNEIntSet' offers a continuation-based interface
--     for deconstructing a 'IntSet' and treating it as if it were an 'NEIntSet'.
--
-- You can convert an 'NEIntSet' into a 'IntSet' with 'toIntSet' or
-- 'Data.IntSet.NonEmpty.IsNonEmpty', essentially "obscuring" the non-empty
-- property from the type.
data NEIntSet =
    NEIntSet { neisV0     :: !Key   -- ^ invariant: must be smaller than smallest value in set
             , neisIntSet :: !IntSet
             }
  deriving (Typeable)

instance Eq NEIntSet where
    t1 == t2  = S.size (neisIntSet t1) == S.size (neisIntSet t2)
             && toList t1 == toList t2

instance Ord NEIntSet where
    compare = compare `on` toList
    (<)     = (<) `on` toList
    (>)     = (>) `on` toList
    (<=)    = (<=) `on` toList
    (>=)    = (>=) `on` toList

instance Show NEIntSet where
    showsPrec p xs = showParen (p > 10) $
      showString "fromList (" . shows (toList xs) . showString ")"

instance Read NEIntSet where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      xs <- readPrec
      return (fromList xs)

    readListPrec = readListPrecDefault

instance NFData NEIntSet where
    rnf (NEIntSet x s) = rnf x `seq` rnf s

-- Data instance code from Data.IntSet.Internal
--
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Joachim Breitner 2011
instance Data NEIntSet where
  gfoldl f z is = z fromList `f` (toList is)
  toConstr _     = fromListConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z fromList)
    _ -> error "gunfold"
  dataTypeOf _   = intSetDataType

fromListConstr :: Constr
fromListConstr = mkConstr intSetDataType "fromList" [] Prefix

intSetDataType :: DataType
intSetDataType = mkDataType "Data.IntSet.Internal.IntSet" [fromListConstr]





-- | /O(log n)/. Smart constructor for an 'NEIntSet' from a 'IntSet'.  Returns
-- 'Nothing' if the 'IntSet' was originally actually empty, and @'Just' n@
-- with an 'NEIntSet', if the 'IntSet' was not empty.
--
-- 'nonEmptyIntSet' and @'maybe' 'Data.IntSet.empty' 'toIntSet'@ form an
-- isomorphism: they are perfect structure-preserving inverses of
-- eachother.
--
-- See 'Data.IntSet.NonEmpty.IsNonEmpty' for a pattern synonym that lets you
-- "match on" the possiblity of a 'IntSet' being an 'NEIntSet'.
--
-- > nonEmptyIntSet (Data.IntSet.fromList [3,5]) == fromList 3:|[5]
nonEmptyIntSet :: IntSet -> Maybe NEIntSet
nonEmptyIntSet = (fmap . uncurry) NEIntSet . S.minView
{-# INLINE nonEmptyIntSet #-}

-- | /O(log n)/.
-- Convert a non-empty set back into a normal possibly-empty map, for usage
-- with functions that expect 'IntSet'.
--
-- Can be thought of as "obscuring" the non-emptiness of the set in its
-- type.  See the 'Data.IntSet.NonEmpty.IsNotEmpty' pattern.
--
-- 'nonEmptyIntSet' and @'maybe' 'Data.IntSet.empty' 'toIntSet'@ form an
-- isomorphism: they are perfect structure-preserving inverses of
-- eachother.
--
-- > toIntSet (fromList ((3,"a") :| [(5,"b")])) == Data.IntSet.fromList [(3,"a"), (5,"b")]
toIntSet :: NEIntSet -> IntSet
toIntSet (NEIntSet x s) = insertMinIntSet x s
{-# INLINE toIntSet #-}

-- | /O(1)/. Create a singleton set.
singleton :: Key -> NEIntSet
singleton x = NEIntSet x S.empty
{-# INLINE singleton #-}

-- | /O(n*log n)/. Create a set from a list of elements.

-- TODO: write manually and optimize to be equivalent to
-- 'fromDistinctAscList' if items are ordered, just like the actual
-- 'S.fromList'.
fromList :: NonEmpty Key -> NEIntSet
fromList (x :| s) = maybe (singleton x) (<> singleton x)
                  . nonEmptyIntSet
                  $ S.fromList s
{-# INLINE fromList #-}

-- | /O(n)/. Convert the set to a non-empty list of elements.
toList :: NEIntSet -> NonEmpty Key
toList (NEIntSet x s) = x :| S.toList s
{-# INLINE toList #-}

-- | /O(m*log(n\/m + 1)), m <= n/. The union of two sets, preferring the first set when
-- equal elements are encountered.
union
    :: NEIntSet
    -> NEIntSet
    -> NEIntSet
union n1@(NEIntSet x1 s1) n2@(NEIntSet x2 s2) = case compare x1 x2 of
    LT -> NEIntSet x1 . S.union s1 . toIntSet $ n2
    EQ -> NEIntSet x1 . S.union s1         $ s2
    GT -> NEIntSet x2 . S.union (toIntSet n1) $ s2
{-# INLINE union #-}

-- | The union of a non-empty list of sets
unions
    :: Foldable1 f
    => f NEIntSet
    -> NEIntSet
unions (F1.toNonEmpty->(s :| ss)) = F.foldl' union s ss
{-# INLINE unions #-}

-- | Left-biased union
instance Semigroup NEIntSet where
    (<>) = union
    {-# INLINE (<>) #-}
    sconcat = unions
    {-# INLINE sconcat #-}

-- | /O(n)/. Test if the internal set structure is valid.
valid :: NEIntSet -> Bool
valid (NEIntSet x s) = all ((x <) . fst) (S.minView s)







-- | /O(log n)/. Insert new value into a set where values are
-- /strictly greater than/ the new values  That is, the new value must be
-- /strictly less than/ all values present in the 'IntSet'.  /The precondition
-- is not checked./
--
-- At the moment this is simply an alias for @Data.IntSet.insert@, but it's
-- left here as a placeholder in case this eventually gets implemented in
-- a more efficient way.

-- TODO: implementation
insertMinIntSet :: Key -> IntSet -> IntSet
insertMinIntSet = S.insert
{-# INLINABLE insertMinIntSet #-}

-- | /O(log n)/. Insert new value into a set where values are /strictly
-- less than/ the new value.  That is, the new value must be /strictly
-- greater than/ all values present in the 'IntSet'.  /The precondition is not
-- checked./
--
-- At the moment this is simply an alias for @Data.IntSet.insert@, but it's
-- left here as a placeholder in case this eventually gets implemented in
-- a more efficient way.

-- TODO: implementation
insertMaxIntSet :: Key -> IntSet -> IntSet
insertMaxIntSet = S.insert
{-# INLINABLE insertMaxIntSet #-}

