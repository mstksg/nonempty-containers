{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Monad
import           Hedgehog
import           Tests.Map
import           System.Exit
import           System.IO

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    results <- checkParallel mapTests

    unless results exitFailure
