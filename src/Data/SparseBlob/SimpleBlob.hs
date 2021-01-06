
-- | Much simpler implementation of the `Data.SparseBlob.SparseBlob` using an
-- `IntMap`. Provides an identical interface. This implementation is used to
-- test the sophisticated implementation.
--
-- This file is included in the source, as it is used by both the test suite and
-- benchmarks.
module Data.SparseBlob.SimpleBlob
  ( -- * Data Structures
    SimpleBlob
    -- * Construction
  , empty
    -- * Insertion
  , insert
  , insertRange
    -- * Deletion
  , deleteRange
    -- * Query
  , lookup
  , lookupRegions
  , lookupRange
  , allRegions
    -- * Conversion
  , toList
  , fromList
  ) where

import Prelude hiding ( lookup )
-- Stdlib imports
import           Data.List ( foldl' )
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty ((:|)) )
import           Data.Word ( Word8 )
import           Data.Maybe ( fromMaybe )
-- Extra stdlib imports
import qualified Data.ByteString as BS
import           Data.ByteString ( ByteString )
import qualified Data.IntMap as IntMap
import           Data.IntMap ( IntMap )
-- Local imports
import qualified Data.SparseBlob.Range as R
import           Data.SparseBlob.Helpers
import           Data.SparseBlob.Range ( Offset, Length )


-- # Data Structures #

type SimpleBlob = IntMap Word8


-- # Construction #

empty :: SimpleBlob
empty = IntMap.empty


-- # Insertion #

insert :: Offset -> Word8 -> SimpleBlob -> SimpleBlob
insert off 0 = IntMap.delete off
insert off v = IntMap.insert off v

insertRange :: Offset -> ByteString -> SimpleBlob -> SimpleBlob
insertRange off xs b = foldr (uncurry insert) b $ zip [off..] (BS.unpack xs)


-- # Deletion #

deleteRange :: Offset -> Length -> SimpleBlob -> SimpleBlob
deleteRange off len b = foldr IntMap.delete b [off..off+len-1]


-- # Query #

lookup :: Offset -> SimpleBlob -> Word8
lookup = fromMaybe 0 .: IntMap.lookup

lookupRegions :: Offset -> Length -> SimpleBlob -> [(Offset, ByteString)]
lookupRegions off len = allRegions . sliceInRange (off, len)

lookupRange :: Offset -> Length -> SimpleBlob -> ByteString
lookupRange off len b = BS.pack $ map (`lookup` b) [off..off+len-1]

allRegions :: SimpleBlob -> [(Offset, ByteString)]
allRegions = groupAdjacent . IntMap.toAscList
  where
  groupAdjacent :: [(Offset,Word8)] -> [(Offset,ByteString)]
  groupAdjacent = map toGroup . groupSliding (\(idx1,_) (idx2,_) -> idx1 + 1 == idx2)
  toGroup :: NonEmpty (Offset,Word8) -> (Offset, ByteString)
  toGroup ((off,x) :| xs) = (off, BS.pack (x : map snd xs))


-- # Conversion #

toList :: SimpleBlob -> [(Offset, ByteString)]
toList = allRegions

fromList :: [(Offset, ByteString)] -> SimpleBlob
fromList = foldl' (flip $ uncurry insertRange) empty


-- # Internal Helpers #

-- | Returns the map containing only the elements with keys in the range.
--
-- >>> sliceInRange (3,2) (IntMap.fromList $ zip [0..] [1,2,3,4,5,6,7,8,9])
-- fromList [(3,4),(4,5)]
sliceInRange :: (Offset, Length) -> IntMap a -> IntMap a
sliceInRange (off, len) = fst . IntMap.split (off+len) . snd . IntMap.split (off-1)
