{-# LANGUAGE ScopedTypeVariables #-}

module Data.SparseBlob.HelpersTest
  ( tests
  ) where

-- Stdlib imports
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty ((:|)), (<|) )
-- External library imports
import           Test.Tasty ( TestTree, testGroup )
import           Test.Tasty.HUnit ( assertEqual, assertBool, testCase )
import qualified Test.Tasty.QuickCheck as Q
import           Test.Tasty.QuickCheck
  ( Gen, Positive, Arbitrary, testProperty, arbitrary )
-- Local library imports
import           Data.SparseBlob.Helpers
  ( maybeBinR, eqLen, groupSliding )


tests :: TestTree
tests =
  testGroup "Helpers"
    [ testGroup "Unit Tests" unitTests
    , testGroup "QuickCheck Tests" quickcheckTests
    ]


-- #############################################################################
-- # # # # # # # # # # # # # # # # Unit Tests  # # # # # # # # # # # # # # # # #
-- #############################################################################

unitTests :: [TestTree]
unitTests =
  [ testCase "maybeBinR Just" $
      assertEqual [] 5 (maybeBinR (+) (Just 2) 3)
  , testCase "maybeBinR Nothing" $
      assertEqual [] 3 (maybeBinR (+) Nothing 3)
  , testCase "groupSliding" $
      assertEqual [] [[1,2,4],[7],[11,12,13],[16]]
        (map NE.toList $ groupSliding (\a b -> b - a <= 2) [1,2,4,7,11,12,13,16])
  ]


-- #############################################################################
-- # # # # # # # # # # # # # # # Property Tests  # # # # # # # # # # # # # # # #
-- #############################################################################

quickcheckTests :: [TestTree]
quickcheckTests =
  [ testGroup "eqLen"
      [ testProperty "behaves like length" $
          \(xs::[Int]) (ys::[Int]) -> (length xs == length ys) == eqLen xs ys
      ]
  , testGroup "groupSliding"
      [ testProperty "elements preserved in order" $
          \(xs::[Int]) ->
            concatMap NE.toList (groupSliding (\a b -> abs (b - a) <= 5) xs) == xs
      ]
  ]
