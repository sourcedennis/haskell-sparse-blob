{-# LANGUAGE ScopedTypeVariables #-}

-- | Unit tests and property tests for `SparseBlob`s.
--
-- Note that this module has access to some internal definitions, which are
-- exposed by `Data.SparseBlobImpl`. (These are externally inaccessible)
module Data.SparseBlobTest where

-- Stdlib imports
import           Data.Word ( Word8 )
-- Extra stdlib imports
import qualified Data.ByteString as BS
import           Data.ByteString ( ByteString )
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
-- External library imports
import           Test.Tasty ( TestTree, testGroup )
import           Test.Tasty.HUnit ( assertEqual, assertBool, testCase )
import qualified Test.Tasty.QuickCheck as Q
import           Test.Tasty.QuickCheck
  ( Gen, Arbitrary, Positive, NonNegative
  , frequency, testProperty, suchThat, arbitrary
  )
-- Local imports
import qualified Data.SparseBlob.Range as R
import           Data.SparseBlob.Range ( Offset, Length, isSubrange )
import qualified Data.SparseBlob.Internal as SB
import           Data.SparseBlob.Internal ( SparseBlob (..), Entry (..), empty )
import qualified Data.SparseBlob.SimpleBlob as SI
import           Data.SparseBlob.SimpleBlob ( SimpleBlob )


tests :: TestTree
tests =
  testGroup "SparseBlob"
    [ testGroup "Unit Tests" unitTests
    , testGroup "QuickCheck Tests" quickcheckTests
    ]


-- #############################################################################
-- # # # # # # # # # # # # # # # # Unit Tests  # # # # # # # # # # # # # # # # #
-- #############################################################################

unitTests :: [TestTree]
unitTests =
  [ testCase "equality reflexive" $
      assertEqual [] (insertRange8 0 [1,2,3,4,5,6] empty) (insertRange8 0 [1,2,3,4,5,6] empty)
  , testCase "zeros implicit" $
      assertEqual [] (insertRange8 0 [0,1,2,3,4,5,6] empty) (insertRange8 1 [1,2,3,4,5,6] empty)
    -- # Internals
  , testGroup "internals"
      [ testCase "cutContained" $
          assertEqual [] [entry 0 [1], entry 2 [3]] (SB.cutContained (1,1) $ entry 0 [1,2,3])
      , testCase "overwriteEntries" $
          assertEqual [] [entry 0 [1], entry 2 [3]] (SB.overwriteEntries (1,1) [] [entry 0 [1,2,3]])
      ]
  ]


-- #############################################################################
-- # # # # # # # # # # # # # # # Property Tests  # # # # # # # # # # # # # # # #
-- #############################################################################

quickcheckTests :: [TestTree]
quickcheckTests =
  [ testProperty "balanced" $
      Q.forAll (SB.fromList <$> genSparse True) isBalanced
  , testProperty "valid height" $
      Q.forAll (SB.fromList <$> genSparse True) isValidHeight
  , testProperty "valid order" $
      Q.forAll (SB.fromList <$> genSparse True) isValidOrder
  , testProperty "valid ranges" $
      Q.forAll (SB.fromList <$> genSparse True) isValidRange
  , testProperty "contains no zeros" $
      Q.forAll (SB.fromList <$> genSparse True) containsNoZeros
  , testProperty "no empty nodes" $
      Q.forAll (SB.fromList <$> genSparse True) isNoneEmpty
  , testProperty "length preserved" $
      Q.forAll (genSparse True) $ \xs ->
        numBytes (SB.fromList xs) == sum (map (BS.length . snd) xs)
  , testProperty "bytes preserved" $
      -- No zero spacing, as it combines consecutive ranges
      Q.forAll (genSparse False) $ \xs ->
        SB.allRegions (SB.fromList xs) == xs
  , testProperty "show/read preserves equality" $
      Q.forAll (genSparse True) $ \xs ->
        let b = SB.fromList xs
        in read (show b) == b
  , testGroup "SimpleBlob comparison"
      [ testProperty "construction" $
          Q.forAll (genSparse True) $ \xs ->
            let sparseBlob = SB.fromList xs
                simpleBlob = SI.fromList xs
            in SB.allRegions sparseBlob == SI.allRegions simpleBlob
      , testProperty "deleteRange" $
          Q.forAll ((,) <$> genSparse True <*> genRange) $ \(xs, (off, len)) ->
            let sparseBlob = SB.deleteRange off len $ SB.fromList xs
                simpleBlob = SI.deleteRange off len $ SI.fromList xs
            in SB.allRegions sparseBlob == SI.allRegions simpleBlob
      , testProperty "lookupRegions" $
          Q.forAll ((,) <$> genSparse True <*> genRange) $ \(xs, (off, len)) ->
            let ys = SB.lookupRegions off len $ SB.fromList xs
                zs = SI.lookupRegions off len $ SI.fromList xs
            in ys == zs
      , testProperty "lookupRange" $
          Q.forAll ((,) <$> genSparse True <*> genRange) $ \(xs, (off, len)) ->
            let ys = SB.lookupRange off len $ SB.fromList xs
                zs = SI.lookupRange off len $ SI.fromList xs
            in ys == zs
      ]
  ]

-- | Returns `True` if the AVL tree is truly balanced.
--
-- A node is balanced iff the height of both children differs by at most one. A
-- search tree is balanced iff all its nodes are balanced.
isBalanced :: SparseBlob -> Bool
isBalanced BlobNil = True
isBalanced n@(BlobNode _ _ l _ r) =
  abs (SB.balanceFactor n) <= 1
    && isBalanced l && isBalanced r

-- | Returns `True` if the height stored in each node is the actual height of
-- the (sub-)tree.
isValidHeight :: SparseBlob -> Bool
isValidHeight BlobNil = True
isValidHeight (BlobNode h _ l _ r) =
  h == max (SB.height l) (SB.height r) + 1
    && isValidHeight l && isValidHeight r

-- | Returns `True` if the contained ranges are disjoint and ordered.
isValidOrder :: SparseBlob -> Bool
isValidOrder = isValidOrder' . SB.allRegions
  where
  isValidOrder' :: [(Offset, ByteString)] -> Bool
  isValidOrder' []  = True
  isValidOrder' [_] = True
  isValidOrder' ((o1,b1):(o2,b2):xs) =
    -- Note that this is less-than (instead of less-than-or-equal) because
    -- there must be a gap between them. If there were no gap, the entries
    -- should have been combined.
    o1 + BS.length b1 < o2 && isValidOrder' ((o2,b2):xs)

-- | Returns `True` iff the range of all `BlobNode`s contain their entries and
-- both child nodes
isValidRange :: SparseBlob -> Bool
isValidRange BlobNil = True
isValidRange (BlobNode h (o,s) l (Entry off xs) r) =
  (off, BS.length xs) `isSubrange` (o,s)
    && l `containedIn` (o,s) && r `containedIn` (o,s)
    && isValidRange l && isValidRange r
  where
  containedIn :: SparseBlob -> (Offset, Length) -> Bool
  containedIn BlobNil  _ = True
  containedIn (BlobNode _ (o1,s1) _ _ _) (o2,s2) =
    (o1,s1) `isSubrange` (o2,s2)

-- | Returns `True` iff the tree contains no explicit zero bytes.
--
-- Note that zeros are explicitly omitted in the representation. (Instead, gaps
-- represent zeros)
containsNoZeros :: SparseBlob -> Bool
containsNoZeros BlobNil = True
containsNoZeros (BlobNode _ _ l (Entry _ xs) r) =
  BS.null (BS.filter (== 0) xs)
    && containsNoZeros l && containsNoZeros r

-- | Returns `True` iff the tree contains no zero-length regions.
isNoneEmpty :: SparseBlob -> Bool
isNoneEmpty BlobNil = True
isNoneEmpty (BlobNode _ _ l (Entry _ xs) r) =
  not (BS.null xs) && isNoneEmpty l && isNoneEmpty r


-- #############################################################################
-- # # # # # # # # # # # # # # # # # Helpers # # # # # # # # # # # # # # # # # #
-- #############################################################################

-- # Generation Helpers #

-- | Generates a random range
genRange :: Gen (Offset, Length)
genRange = (,) <$> suchThat genNonNegative (< 1000) <*> suchThat genPositive (< 200)

-- | Generates sparse byte regions. The gaps between the elements represent
-- zeros. The generated regions are disjoint and sorted by offset.
--
-- When empty spacing is allowed, the generated regions may be directly
-- adjacent; For example, @[(0,[1,2]),(2,[3,4])@.
genSparse :: Bool -> Gen [(Offset, ByteString)]
genSparse allowNoSpacing = genSparse' 0
  where
  -- | Spacing generator which obeys the boolean parameter.
  genSpacing :: Gen Int
  genSpacing
    | allowNoSpacing  = genNonNegative
    | otherwise       = genPositive
  -- | Generates a list of sparse regions, where the first surely starts /after/
  -- the given offset.
  genSparse' :: Offset -> Gen [(Offset, ByteString)]
  genSparse' off =
    frequency
      [ ( 1, pure [] )
      , ( 9,
          do
            spacing <- suchThat genSpacing (< 1000)
            len     <- suchThat genPositive (< 200)
            xs      <- BS.pack <$> genBytes len
            ((off + spacing, xs):) <$> genSparse' (off + spacing + len)
        )
      ]
  -- | Generates a list of non-zero bytes. Note that these are explicitly
  -- non-zero, as zeros are represented by gaps which the spacing handles.
  genBytes :: Length -> Gen [Word8]
  genBytes 0 = return []
  genBytes n = (:) <$> genPositive <*> genBytes (n-1)


-- # Other #

-- | Returns the number of non-zero bytes contained in the BLOB.
numBytes :: SparseBlob -> Int
numBytes BlobNil = 0
numBytes (BlobNode _ _ l e r) = numBytes l + SB.entryLen e + numBytes r

-- | Generates a numeric value which is strictly positive.
genPositive :: forall a . (Num a, Ord a, Arbitrary a) => Gen a
genPositive = Q.getPositive <$> (arbitrary :: Gen (Positive a))

-- | Generates a non-negative numeric value.
genNonNegative :: forall a . (Num a, Ord a, Arbitrary a) => Gen a
genNonNegative = Q.getNonNegative <$> (arbitrary :: Gen (NonNegative a))

-- | Constructs an entry from a list of bytes. Notational convenience.
entry :: Offset -> [Word8] -> Entry
entry off xs = Entry off (BS.pack xs)

-- | Inserts bytes into the BLOB at the given offset. Notational convenience.
insertRange8 :: Offset -> [Word8] -> SparseBlob -> SparseBlob
insertRange8 off xs = SB.insertRange off (BS.pack xs)
