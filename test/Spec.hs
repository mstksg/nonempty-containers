
-- import           Test.Tasty.Hedgehog
-- import           Test.Tasty.Ingredients.ConsoleReporter
import           Test.Tasty
import           Tests.IntMap
import           Tests.IntSet
import           Tests.Map
import           Tests.Sequence
import           Tests.Set

setOpts :: TestTree -> TestTree
setOpts = id
-- setOpts = localOption (HedgehogTestLimit    (Just 500))
--         . localOption (HedgehogDiscardLimit (Just 500))
--         . localOption (HideSuccesses        True      )

main :: IO ()
main = defaultMain . setOpts $
            testGroup "Tests" [ mapTests
                              , setTests
                              , intMapTests
                              , intSetTests
                              , sequenceTests
                              ]

