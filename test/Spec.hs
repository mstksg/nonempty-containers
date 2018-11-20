{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Monad
import           Data.Foldable
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Map.NonEmpty  (NEMap, Map(..))
import           Hedgehog
import           Map
import           System.Exit
import           System.IO
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as M
import qualified Data.Map.NonEmpty  as NEM
import qualified Data.Set           as S
import qualified Data.Text          as T
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    results <- checkParallel mapTests

    unless results exitFailure
