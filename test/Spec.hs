{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Data.Bifunctor
import           Control.Monad
import           Hedgehog
import           Tests.Map
import           System.Exit
import           System.IO

tryGroup :: (forall a. Num a => a) -> Group -> Group
tryGroup n Group{..} =
    Group groupName
          ((map . second) (withDiscards n . withTests n)
                          groupProperties
          )

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    results <- checkParallel (tryGroup 1000 mapTests)

    unless results exitFailure

