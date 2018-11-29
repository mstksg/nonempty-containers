{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Tests.Sequence (sequenceTests) where

import           Control.Applicative
import           Control.Comonad
import           Control.Monad
import           Data.Bifunctor
import           Data.Functor.Identity
import           Data.Ord
import           Data.Sequence                   (Seq(..))
import           Data.Sequence.NonEmpty          (NESeq(..))
import           Data.Tuple
import           Hedgehog
import           Test.Tasty
import           Tests.Util
import qualified Data.Foldable                   as F
import qualified Data.List.NonEmpty              as NE
import qualified Data.Semigroup.Foldable         as F1
import qualified Data.Semigroup.Traversable      as T1
import qualified Data.Sequence                   as Seq
import qualified Data.Sequence.NonEmpty          as NESeq
import qualified Data.Sequence.NonEmpty.Internal as NESeq
import qualified Hedgehog.Gen                    as Gen

sequenceTests :: TestTree
sequenceTests = groupTree $$(discover)

prop_toSeqIso1 :: Property
prop_toSeqIso1 = property $ do
    m0 <- forAll seqGen
    tripping m0 NESeq.nonEmptySeq
                (Identity . maybe Seq.empty NESeq.toSeq)

prop_toSeqIso2 :: Property
prop_toSeqIso2 = property $ do
    m0 <- forAll $ Gen.maybe neSeqGen
    tripping m0 (maybe Seq.empty NESeq.toSeq)
                (Identity . NESeq.nonEmptySeq)

prop_read_show :: Property
prop_read_show = readShow neSeqGen

prop_read1_show1 :: Property
prop_read1_show1 = readShow1 neSeqGen

prop_show_show1 :: Property
prop_show_show1 = showShow1 neSeqGen





prop_cons :: Property
prop_cons = ttProp (GTVal :-> GTSeq :-> TTNESeq)
    (:<|)
    (:<||)

prop_snoc :: Property
prop_snoc = ttProp (GTSeq :-> GTVal :-> TTNESeq)
    (:|>)
    (:||>)

prop_insertSeqAt :: Property
prop_insertSeqAt = ttProp (GTIntKey :-> GTVal :-> GTSeq :-> TTNESeq)
    Seq.insertAt
    NESeq.insertSeqAt

prop_singleton :: Property
prop_singleton = ttProp (GTVal :-> TTNESeq)
    Seq.singleton
    NESeq.singleton

prop_consNE :: Property
prop_consNE = ttProp (GTVal :-> GTNESeq :-> TTNESeq)
    (Seq.<|)
    (NESeq.<|)

prop_snocNE :: Property
prop_snocNE = ttProp (GTNESeq :-> GTVal :-> TTNESeq)
    (Seq.|>)
    (NESeq.|>)

prop_append :: Property
prop_append = ttProp (GTNESeq :-> GTNESeq :-> TTNESeq)
    (Seq.><)
    (NESeq.><)

prop_appendL :: Property
prop_appendL = ttProp (GTNESeq :-> GTSeq :-> TTNESeq)
    (Seq.><)
    (NESeq.|><)

prop_appendR :: Property
prop_appendR = ttProp (GTSeq :-> GTNESeq :-> TTNESeq)
    (Seq.><)
    (NESeq.><|)

prop_fromList :: Property
prop_fromList = ttProp (GTNEList Nothing GTVal :-> TTNESeq)
    Seq.fromList
    NESeq.fromList

prop_fromFunction :: Property
prop_fromFunction = ttProp (GTSize :-> gf1 valGen :?> TTNESeq)
    (Seq.fromFunction   . (+ 1))
    (NESeq.fromFunction . (+ 1))

prop_replicate :: Property
prop_replicate = ttProp (GTSize :-> GTVal :-> TTNESeq)
    (Seq.replicate   . (+ 1))
    (NESeq.replicate . (+ 1))

prop_replicateA :: Property
prop_replicateA = ttProp (GTSize :-> GTVal :-> TTBazaar GTVal TTNESeq TTVal)
    (\i x -> Seq.replicateA   (i + 1) (x `More` Done id))
    (\i x -> NESeq.replicateA (i + 1) (x `More` Done id))

prop_replicateA1 :: Property
prop_replicateA1 = ttProp (GTSize :-> GTVal :-> TTBazaar GTVal TTNESeq TTVal)
    (\i x -> Seq.replicateA    (i + 1) (x `More` Done id))
    (\i x -> NESeq.replicateA1 (i + 1) (x `More` Done id))

prop_cycleTaking :: Property
prop_cycleTaking = ttProp (GTSize :-> GTNESeq :-> TTNESeq)
    (Seq.cycleTaking   . (* 5) . (+ 1))
    (NESeq.cycleTaking . (* 5) . (+ 1))

prop_iterateN :: Property
prop_iterateN = ttProp (GTSize :-> gf1 valGen :?> GTVal :-> TTNESeq)
    (Seq.iterateN   . (+ 1))
    (NESeq.iterateN . (+ 1))

prop_unfoldr :: Property
prop_unfoldr = ttProp ( GTSize
                    :-> gf1 ((,) <$> valGen <*> Gen.maybe intKeyGen)
                    :?> GTIntKey
                    :-> TTNESeqList
                      )
    (\i f -> NE.unfoldr    (limiter f) . (i,))
    (\i f -> NESeq.unfoldr (limiter f) . (i,))

prop_unfoldl :: Property
prop_unfoldl = ttProp ( GTSize
                    :-> gf1 ((,) <$> valGen <*> Gen.maybe intKeyGen)
                    :?> GTIntKey
                    :-> TTNESeqList
                      )
    (\i f -> NE.reverse . NE.unfoldr    (       limiter f) . (i,))
    (\i f ->              NESeq.unfoldl (swap . limiter f) . (i,))

limiter
    :: (a -> (b, Maybe a))
    -> (Int, a)
    -> (b, Maybe (Int, a))
limiter f (n, x) = second (go =<<) $ f x
  where
    go y
      | n <= 0    = Nothing
      | otherwise = Just (n - 1, y)

prop_head :: Property
prop_head = ttProp (GTNESeq :-> TTMaybe TTVal)
    (\case x :<| _ -> Just x; Empty -> Nothing)
    (Just . NESeq.head)

prop_tail :: Property
prop_tail = ttProp (GTNESeq :-> TTMaybe TTOther)
    (\case _ :<| xs -> Just xs; Empty -> Nothing)
    (Just . NESeq.tail)

prop_last :: Property
prop_last = ttProp (GTNESeq :-> TTMaybe TTVal)
    (\case _ :|> x -> Just x; Empty -> Nothing)
    (Just . NESeq.last)

prop_init :: Property
prop_init = ttProp (GTNESeq :-> TTMaybe TTOther)
    (\case xs :|> _ -> Just xs; Empty -> Nothing)
    (Just . NESeq.init)

prop_length :: Property
prop_length = ttProp (GTNESeq :-> TTOther)
    Seq.length
    NESeq.length

prop_scanl :: Property
prop_scanl = ttProp (gf2 valGen :?> GTVal :-> GTNESeq :-> TTNESeq)
    Seq.scanl
    NESeq.scanl

prop_scanl1 :: Property
prop_scanl1 = ttProp (gf2 valGen :?> GTNESeq :-> TTNESeq)
    Seq.scanl1
    NESeq.scanl1

prop_scanr :: Property
prop_scanr = ttProp (gf2 valGen :?> GTVal :-> GTNESeq :-> TTNESeq)
    Seq.scanr
    NESeq.scanr

prop_scanr1 :: Property
prop_scanr1 = ttProp (gf2 valGen :?> GTNESeq :-> TTNESeq)
    Seq.scanl1
    NESeq.scanl1

prop_tails :: Property
prop_tails = ttProp (GTNESeq :-> TTNESeq)
    (Seq.filter (not . null) . Seq.tails)
    (fmap NESeq.toSeq . NESeq.tails)

prop_inits :: Property
prop_inits = ttProp (GTNESeq :-> TTNESeq)
    (Seq.filter (not . null) . Seq.inits)
    (fmap NESeq.toSeq . NESeq.inits)

prop_chunksOf :: Property
prop_chunksOf = ttProp (GTSize :-> GTNESeq :-> TTNESeq)
    (\i -> Seq.filter (not . null) . Seq.chunksOf   (i + 1))
    (\i -> fmap NESeq.toSeq        . NESeq.chunksOf (i + 1))

prop_takeWhileL :: Property
prop_takeWhileL = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTOther)
    Seq.takeWhileL
    NESeq.takeWhileL

prop_takeWhileR :: Property
prop_takeWhileR = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTOther)
    Seq.takeWhileR
    NESeq.takeWhileR

prop_dropWhileL :: Property
prop_dropWhileL = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTOther)
    Seq.dropWhileL
    NESeq.dropWhileL

prop_dropWhileR :: Property
prop_dropWhileR = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTOther)
    Seq.dropWhileR
    NESeq.dropWhileR

prop_spanl :: Property
prop_spanl = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTThese TTNESeq TTNESeq)
    Seq.spanl
    NESeq.spanl

prop_spanr :: Property
prop_spanr = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTThese TTNESeq TTNESeq)
    Seq.spanr
    NESeq.spanr

prop_breakl :: Property
prop_breakl = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTThese TTNESeq TTNESeq)
    Seq.breakl
    NESeq.breakl

prop_breakr :: Property
prop_breakr = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTThese TTNESeq TTNESeq)
    Seq.breakr
    NESeq.breakr

prop_partition :: Property
prop_partition = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTThese TTNESeq TTNESeq)
    Seq.partition
    NESeq.partition

prop_filter :: Property
prop_filter = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTOther)
    Seq.filter
    NESeq.filter

prop_sort :: Property
prop_sort = ttProp (GTNESeq :-> TTNESeq)
    Seq.sort
    NESeq.sort

prop_sortBy :: Property
prop_sortBy = ttProp (gf1 valGen :?> GTNESeq :-> TTNESeq)
    (Seq.sortBy   . comparing)
    (NESeq.sortBy . comparing)

prop_sortOn :: Property
prop_sortOn = ttProp (gf1 valGen :?> GTNESeq :-> TTNESeq)
    NESeq.sortOnSeq
    NESeq.sortOn

prop_unstableSort :: Property
prop_unstableSort = ttProp (GTNESeq :-> TTNESeq)
    Seq.unstableSort
    NESeq.unstableSort

prop_unstableSortBy :: Property
prop_unstableSortBy = ttProp (gf1 valGen :?> GTNESeq :-> TTNESeq)
    (Seq.unstableSortBy   . comparing)
    (NESeq.unstableSortBy . comparing)

prop_unstableSortOn :: Property
prop_unstableSortOn = ttProp (gf1 valGen :?> GTNESeq :-> TTNESeq)
    NESeq.unstableSortOnSeq
    NESeq.unstableSortOn

prop_lookup :: Property
prop_lookup = ttProp (GTIntKey :-> GTNESeq :-> TTMaybe TTVal)
    Seq.lookup
    NESeq.lookup

prop_index :: Property
prop_index = ttProp (GTNESeq :-> GTIntKey :-> TTVal)
    (\xs i -> xs `Seq.index`   (i `mod` Seq.length xs  ))
    (\xs i -> xs `NESeq.index` (i `mod` NESeq.length xs))

prop_adjust :: Property
prop_adjust = ttProp (gf1 valGen :?> GTIntKey :-> GTNESeq :-> TTNESeq)
    Seq.adjust
    NESeq.adjust

prop_adjust' :: Property
prop_adjust' = ttProp (gf1 valGen :?> GTIntKey :-> GTNESeq :-> TTNESeq)
    Seq.adjust'
    NESeq.adjust'

prop_update :: Property
prop_update = ttProp (GTIntKey :-> GTVal :-> GTNESeq :-> TTNESeq)
    Seq.update
    NESeq.update

prop_take :: Property
prop_take = ttProp (GTIntKey :-> GTNESeq :-> TTOther)
    Seq.take
    NESeq.take

prop_drop :: Property
prop_drop = ttProp (GTIntKey :-> GTNESeq :-> TTOther)
    Seq.drop
    NESeq.drop

prop_insertAt :: Property
prop_insertAt = ttProp (GTIntKey :-> GTVal :-> GTNESeq :-> TTNESeq)
    Seq.insertAt
    NESeq.insertAt

prop_deleteAt :: Property
prop_deleteAt = ttProp (GTIntKey :-> GTNESeq :-> TTOther)
    Seq.deleteAt
    NESeq.deleteAt

prop_splitAt :: Property
prop_splitAt = ttProp (GTIntKey :-> GTNESeq :-> TTThese TTNESeq TTNESeq)
    Seq.splitAt
    NESeq.splitAt

prop_elemIndexL :: Property
prop_elemIndexL = ttProp (GTVal :-> GTNESeq :-> TTOther)
    Seq.elemIndexL
    NESeq.elemIndexL

prop_elemIndicesL :: Property
prop_elemIndicesL = ttProp (GTVal :-> GTNESeq :-> TTOther)
    Seq.elemIndicesL
    NESeq.elemIndicesL

prop_elemIndexR :: Property
prop_elemIndexR = ttProp (GTVal :-> GTNESeq :-> TTOther)
    Seq.elemIndexR
    NESeq.elemIndexR

prop_elemIndicesR :: Property
prop_elemIndicesR = ttProp (GTVal :-> GTNESeq :-> TTOther)
    Seq.elemIndicesR
    NESeq.elemIndicesR

prop_findIndexL :: Property
prop_findIndexL = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTOther)
    Seq.findIndexL
    NESeq.findIndexL

prop_findIndicesL :: Property
prop_findIndicesL = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTOther)
    Seq.findIndicesL
    NESeq.findIndicesL

prop_findIndexR :: Property
prop_findIndexR = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTOther)
    Seq.findIndexR
    NESeq.findIndexR

prop_findIndicesR :: Property
prop_findIndicesR = ttProp (gf1 Gen.bool :?> GTNESeq :-> TTOther)
    Seq.findIndicesR
    NESeq.findIndicesR

prop_foldMapWithIndex :: Property
prop_foldMapWithIndex = ttProp (gf2 valGen :?> GTNESeq :-> TTOther)
    (\f -> Seq.foldMapWithIndex   (\i -> (:[]) . f i))
    (\f -> NESeq.foldMapWithIndex (\i -> (:[]) . f i))

prop_foldlWithIndex :: Property
prop_foldlWithIndex = ttProp (gf3 valGen :?> GTVal :-> GTNESeq :-> TTVal)
    Seq.foldlWithIndex
    NESeq.foldlWithIndex

prop_foldrWithIndex :: Property
prop_foldrWithIndex = ttProp (gf3 valGen :?> GTVal :-> GTNESeq :-> TTVal)
    Seq.foldrWithIndex
    NESeq.foldrWithIndex

prop_mapWithIndex :: Property
prop_mapWithIndex = ttProp (gf2 valGen :?> GTNESeq :-> TTNESeq)
    Seq.mapWithIndex
    NESeq.mapWithIndex

prop_traverseWithIndex :: Property
prop_traverseWithIndex = ttProp (gf2 valGen :?> GTNESeq :-> TTBazaar GTVal TTNESeq TTVal)
    (\f -> Seq.traverseWithIndex   (\k -> (`More` Done (f k))))
    (\f -> NESeq.traverseWithIndex (\k -> (`More` Done (f k))))

prop_traverseWithIndex1 :: Property
prop_traverseWithIndex1 = ttProp (gf2 valGen :?> GTNESeq :-> TTBazaar GTVal TTNESeq TTVal)
    (\f -> Seq.traverseWithIndex    (\k -> (`More` Done (f k))))
    (\f -> NESeq.traverseWithIndex1 (\k -> (`More` Done (f k))))

prop_reverse :: Property
prop_reverse = ttProp (GTNESeq :-> TTNESeq)
    Seq.reverse
    NESeq.reverse

prop_intersperse :: Property
prop_intersperse = ttProp (GTVal :-> GTNESeq :-> TTNESeq)
    Seq.intersperse
    NESeq.intersperse

prop_zip :: Property
prop_zip = ttProp (GTNESeq :-> GTNESeq :-> TTNESeq)
    Seq.zip
    NESeq.zip

prop_zipWith :: Property
prop_zipWith = ttProp (gf2 valGen :?> GTNESeq :-> GTNESeq :-> TTNESeq)
    Seq.zipWith
    NESeq.zipWith

prop_zip3 :: Property
prop_zip3 = ttProp (GTNESeq :-> GTNESeq :-> GTNESeq :-> TTNESeq)
    Seq.zip3
    NESeq.zip3

prop_zipWith3 :: Property
prop_zipWith3 = ttProp (gf3 valGen :?> GTNESeq :-> GTNESeq :-> GTNESeq :-> TTNESeq)
    Seq.zipWith3
    NESeq.zipWith3

prop_zip4 :: Property
prop_zip4 = ttProp (GTNESeq :-> GTNESeq :-> GTNESeq :-> GTNESeq :-> TTNESeq)
    Seq.zip4
    NESeq.zip4

prop_zipWith4 :: Property
prop_zipWith4 = ttProp (gf4 valGen :?> GTNESeq :-> GTNESeq :-> GTNESeq :-> GTNESeq :-> TTNESeq)
    Seq.zipWith4
    NESeq.zipWith4

prop_unzip :: Property
prop_unzip = ttProp (GTNESeq :-> GTNESeq :-> TTNESeq :*: TTNESeq)
    (\xs -> NESeq.unzipSeq . Seq.zip   xs)
    (\xs -> NESeq.unzip    . NESeq.zip xs)

prop_unzipWith :: Property
prop_unzipWith = ttProp ( gf1 ((,) <$> valGen <*> valGen)
                      :?> GTNESeq
                      :-> TTNESeq :*: TTNESeq
                        )
    NESeq.unzipWithSeq
    NESeq.unzipWith

prop_liftA2 :: Property
prop_liftA2 = ttProp (gf2 valGen :?> GTNESeq :-> GTNESeq :-> TTNESeq)
    liftA2
    liftA2

prop_liftM2 :: Property
prop_liftM2 = ttProp (gf2 valGen :?> GTNESeq :-> GTNESeq :-> TTNESeq)
    liftM2
    liftM2

prop_duplicate :: Property
prop_duplicate = ttProp (GTNESeqList :-> TTNESeqList)
    duplicate
    (fmap F1.toNonEmpty . duplicate)

prop_foldMap :: Property
prop_foldMap = ttProp (gf1 valGen :?> GTNESeq :-> TTOther)
    (foldMap . fmap (:[]))
    (foldMap . fmap (:[]))

prop_foldl :: Property
prop_foldl = ttProp (gf2 valGen :?> GTVal :-> GTNESeq :-> TTVal)
    foldl
    foldl

prop_foldr :: Property
prop_foldr = ttProp (gf2 valGen :?> GTVal :-> GTNESeq :-> TTVal)
    foldr
    foldr

prop_foldl' :: Property
prop_foldl' = ttProp (gf2 valGen :?> GTVal :-> GTNESeq :-> TTVal)
    F.foldl'
    F.foldl'

prop_foldr' :: Property
prop_foldr' = ttProp (gf2 valGen :?> GTVal :-> GTNESeq :-> TTVal)
    F.foldr'
    F.foldr'

prop_foldl1 :: Property
prop_foldl1 = ttProp (gf2 valGen :?> GTNESeq :-> TTVal)
    foldl1
    foldl1

prop_foldr1 :: Property
prop_foldr1 = ttProp (gf2 valGen :?> GTNESeq :-> TTVal)
    foldr1
    foldr1

prop_fold :: Property
prop_fold = ttProp (GTNESeq :-> TTVal)
    F.fold
    F.fold

prop_fold1 :: Property
prop_fold1 = ttProp (GTNESeq :-> TTVal)
    F.fold
    F1.fold1

prop_toList :: Property
prop_toList = ttProp (GTNESeq :-> TTOther)
    F.toList
    F.toList

prop_toNonEmpty :: Property
prop_toNonEmpty = ttProp (GTNESeq :-> TTNEList TTVal)
    F.toList
    F1.toNonEmpty

prop_sequenceA :: Property
prop_sequenceA = ttProp (GTNESeq :-> TTBazaar GTVal TTNESeq TTVal)
    (sequenceA . fmap (`More` Done id))
    (sequenceA . fmap (`More` Done id))

prop_sequence1 :: Property
prop_sequence1 = ttProp (GTNESeq :-> TTBazaar GTVal TTNESeq TTVal)
    (sequenceA . fmap (`More` Done id))
    (T1.sequence1 . fmap (`More` Done id))
