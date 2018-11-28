import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.Ingredients.ConsoleReporter
import           Tests.IntMap
import           Tests.IntSet
import           Tests.Map
import           Tests.Set

setOpts :: TestTree -> TestTree
setOpts = localOption (HedgehogTestLimit    (Just 500))
        . localOption (HedgehogDiscardLimit (Just 500))
        . localOption (HideSuccesses        True      )

main :: IO ()
main = defaultMain . setOpts $
            testGroup "Tests" [ mapTests
                              , setTests
                              , intMapTests
                              , intSetTests
                              ]

