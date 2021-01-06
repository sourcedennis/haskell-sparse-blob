{-# LANGUAGE ScopedTypeVariables #-}

module Data.SparseBlob.RangeTest
  ( tests
  ) where

-- External library imports
import           Test.Tasty ( TestTree, testGroup )
import           Test.Tasty.HUnit ( assertEqual, assertBool, testCase )
import qualified Test.Tasty.QuickCheck as Q
import           Test.Tasty.QuickCheck ( Gen, Positive, Arbitrary, testProperty, arbitrary )
-- Local library imports
import qualified Data.SparseBlob.Range as R
import           Data.SparseBlob.Range ( Offset, Length )


tests :: TestTree
tests =
  testGroup "Range"
    [ testGroup "Unit Tests" unitTests
    , testGroup "QuickCheck Tests" quickcheckTests
    ]


-- #############################################################################
-- # # # # # # # # # # # # # # # # Unit Tests  # # # # # # # # # # # # # # # # #
-- #############################################################################

unitTests :: [TestTree]
unitTests =
  [ testCase "member" $
      assertBool [] (R.member 3 (2,2))
  , testCase "not member" $
      assertBool [] (not $ R.member 3 (0,3))
  , testCase "union disjoint" $
      assertEqual [] (2,8) (R.union (2,3) (8,2))
  , testCase "union contained" $
      assertEqual [] (2,12) (R.union (2,12) (5,2))
  , testCase "intersect disjoint" $
      assertEqual [] Nothing (R.intersect (2,4) (8,2))
  , testCase "intersect touching" $
      assertEqual [] Nothing (R.intersect (2,4) (6,2))
  , testCase "intersect overlapping" $
      assertEqual [] (Just (4,2)) (R.intersect (2,4) (4,8))
  , testCase "intersect contained" $
      assertEqual [] (Just (2,5)) (R.intersect (2,5) (1,13))
  , testCase "overlaps" $
      assertBool [] (R.overlaps (2,3) (4,6))
  , testCase "not overlaps touching" $
      assertBool [] (not $ R.overlaps (2,3) (5,2))
  , testCase "not overlaps empty" $
      assertBool [] (not $ R.overlaps (2,10) (5,0))
  , testCase "not overlaps flipped empty" $
      assertBool [] (not $ R.overlaps (5,0) (2,10))
  , testCase "subregion" $
      assertBool [] (R.isSubrange (2,5) (0,10))
  , testCase "subregion identical" $
      assertBool [] (R.isSubrange (2,5) (2,5))
  , testCase "subregion left touch" $
      assertBool [] (R.isSubrange (2,5) (2,8))
  , testCase "subregion right touch" $
      assertBool [] (R.isSubrange (4,3) (2,5))
  , testCase "subregion empty" $
      assertBool [] (R.isSubrange (1,0) (5,3))
  , testCase "not subregion touching" $
      assertBool [] (not $ R.isSubrange (1,4) (2,4))
  , testCase "not subregion disjoint" $
      assertBool [] (not $ R.isSubrange (1,2) (4,5))
  ]


-- #############################################################################
-- # # # # # # # # # # # # # # # Property Tests  # # # # # # # # # # # # # # # #
-- #############################################################################

quickcheckTests :: [TestTree]
quickcheckTests =
  [ testProperty "union commutative" $
      Q.forAll ((,) <$> genRange <*> genRange) $ propCommutative R.union
  , testProperty "intersect commutative" $
      Q.forAll ((,) <$> genRange <*> genRange) $ propCommutative R.intersect
  ]

propCommutative :: Eq b => ( a -> a -> b ) -> (a, a) -> Bool
propCommutative f (x,y) = f x y == f y x

genRange :: Gen (Offset, Length)
genRange = (,) <$> (arbitrary :: Gen Offset) <*> (genPositive :: Gen Length)

genPositive :: forall a . (Num a, Ord a, Arbitrary a) => Gen a
genPositive = Q.getPositive <$> (arbitrary :: Gen (Positive a))
