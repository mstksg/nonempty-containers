import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Tests.IntSet
import           Tests.Map
import           Tests.Set

setOpts :: TestTree -> TestTree
setOpts = localOption (HedgehogTestLimit    (Just 250))
        . localOption (HedgehogDiscardLimit (Just 250))

main :: IO ()
main = defaultMain . setOpts $
            testGroup "Tests" [ mapTests
                              , setTests
                              , intMapTests
                              , intSetTests
                              ]

