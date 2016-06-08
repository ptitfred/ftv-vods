import qualified PlaylistManagerTest
import qualified HelpersTest

import Test.Tasty

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "All"
  [ PlaylistManagerTest.suite
  , HelpersTest.suite
  ]
