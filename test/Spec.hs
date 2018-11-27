import           Test.Tasty
import           Tests.Map
import           Tests.Set

main :: IO ()
main = defaultMain $ testGroup "Tests" [ mapTests
                                       , setTests
                                       ]

