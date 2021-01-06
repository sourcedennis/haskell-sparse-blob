
-- External library imports
import Test.Tasty       ( defaultMain, testGroup, localOption )
import Test.Tasty.HUnit ( assertEqual, assertBool, testCase )
import Test.Tasty.QuickCheck ( QuickCheckVerbose (..) )
-- Local imports
import qualified Data.SparseBlobTest as SparseBlobTest
import qualified Data.SparseBlob.RangeTest as RangeTest
import qualified Data.SparseBlob.HelpersTest as HelpersTest


main :: IO ()
main =
  defaultMain $
    --localOption (QuickCheckVerbose True) $
      testGroup "Modules"
        [ SparseBlobTest.tests
        , RangeTest.tests
        , HelpersTest.tests
        ]
