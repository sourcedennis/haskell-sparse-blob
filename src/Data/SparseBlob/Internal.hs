
-- | Internals of `Data.SparseBlob`.
--
-- These definitions are included here such that the internals are exposed to
-- the test suite, without exposing them to the library user.
--
-- For module documentation, see `Data.SparseBlob`.
module Data.SparseBlob.Internal
  ( -- * Data Structures
    SparseBlob (..)
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
    -- * Internals (exposed for testing)
    -- ** Data Structures
  , Entry (..)
    -- ** Type Aliases
  , Height
    -- ** Helpers
  , extractMin
  , extractMax
  , rebuildNode
  , buildBalanced
  , allEntries
  , overwriteEntries
  , cutOverflow
  , cutContained
  , entryLen
  , balanceFactor
  , height
  ) where

import Prelude hiding ( lookup )
-- Stdlib imports
import           Data.Word ( Word8 )
import           Data.Maybe ( fromMaybe, fromJust, mapMaybe )
import           Data.List ( foldl' )
import qualified Data.List.NonEmpty as NE
import           Data.List.NonEmpty ( NonEmpty ((:|)), (<|) )
-- Extra stdlib imports
import qualified Data.ByteString as BS
import           Data.ByteString ( ByteString )
-- Local imports
import           Data.SparseBlob.Helpers
import qualified Data.SparseBlob.Range as R
import           Data.SparseBlob.Range ( Offset, Length )


-- # Data Structures #

-- | A Binary Large OBject (BLOB). A BLOB is sparse when it contains mainly zero
-- values, with occasional non-zero regions.
--
-- BLOBs are (conceptually) unbounded; though, a bound is implicitly enforced by
-- the bounds on `Int`. Explicit bounds should be externally enforced.
data SparseBlob
  = BlobNil
  | BlobNode Height (Offset, Length) SparseBlob Entry SparseBlob


-- # Internal Data Structures #

-- | The height of an AVL tree, which is the largest number of /edges/ to a
-- leaf. The height of a nil-leaf is 0.
type Height = Int

-- | A region of non-zero bytes, which is contained at a node in the AVL tree.
data Entry = Entry Offset ByteString
  deriving Eq


-- # Construction #

-- | The empty BLOB, which contains only zeros.
empty :: SparseBlob
empty = BlobNil


-- # Insertion #

-- | /O(log n)/. Inserts a single byte into the BLOB at the specified offset.
--
-- When inserting multiple consecutive bytes, `insertRange` is preferred.
--
-- /n/ is the number of non-zero byte region contained in the BLOB (which is
-- bounded by the number non non-zero /bytes/ contained).
insert :: Offset -> Word8 -> SparseBlob -> SparseBlob
insert off 0 = overwriteRange (off, 1) []
insert off v = overwriteRange (off, 1) [Entry off $ BS.singleton v]

-- | /O(m log (n+m))/. Inserts multiple consecutive bytes into the BLOB at the
-- specified offset.
--
-- /n/ is the number of non-zero byte /regions/ contained in the BLOB. /m/ is the
-- number of non-zero byte regions /inserted/. Both regions are bounded by their
-- number of bytes contained or inserted, respectively.
insertRange :: Offset -> ByteString -> SparseBlob -> SparseBlob
insertRange off xs =
  overwriteRange (off, BS.length xs) (toEntries off xs)
  where
  -- | /O(n)/. Converts the bytestring into a list of entries. Zero bytes are
  -- omitted within the entries.
  toEntries :: Offset -> ByteString -> [Entry]
  toEntries i xs =
    case BS.uncons xs of
      Nothing -> [] -- All bytes consumed
      Just (0, _) ->
        let (xsZeros, xs') = BS.span (== 0) xs
        in toEntries (i + BS.length xsZeros) xs'
      Just _ ->
        let (xsNonZeros, xs') = BS.span (/= 0) xs
        in Entry i xsNonZeros : toEntries (i + BS.length xsNonZeros) xs'


-- # Deletion #

-- | /O(m log n)/. Deletes the bytes in the specified range. Intuitively, this
-- writes zeros to all bytes in the range.
--
-- /n/ is the number of non-zero byte /regions/ contained in the BLOB. /m/ is the
-- number of non-zero byte regions located within the /deleted range/, which is
-- bounded by /m/ (a region of /m/ bytes never contains more than /m\/2/
-- non-zero regions).
deleteRange :: Offset -> Length -> SparseBlob -> SparseBlob
deleteRange off len = overwriteRange (off, len) []


-- # Query #

-- | /O(log n)/. Returns the value of the byte located at the given offset
-- within the BLOB.
--
-- /n/ is the number of non-zero byte /regions/ contained in the BLOB (which is
-- bounded by the number non non-zero /bytes/ contained).
lookup :: Offset -> SparseBlob -> Word8
lookup _   BlobNil = 0
lookup idx (BlobNode _ (o,s) l (Entry xsOff xs) r)
  | idx < o                     = 0
  | idx < xsOff                 = lookup idx l
  | idx < xsOff + BS.length xs  = fromJust $ byteAtIndex (idx - xsOff) xs
  | idx < o + s                 = lookup idx r
  | otherwise                   = 0

-- | /O(m log n)/. Returns the (sorted and disjoint) byte sections within the
-- range. The gaps between regions represent zero-regions.
--
-- /n/ is the number of non-zero byte /regions/ contained in the BLOB. /m/ is
-- the number of non-zero regions contained (and returned) within the range.
-- Both are bounded by their contained number of bytes.
lookupRegions :: Offset -> Length -> SparseBlob -> [(Offset, ByteString)]
lookupRegions off len =
  mapMaybe (fmap unEntry . cutOverflow (off, len)) . fst . extractEntries (off, len)

-- | /O(m log n)/. Returns the byte sequence within the range.
--
-- Using `lookupRegions` is preferred, as it likely contains fewer bytes;
-- considering gaps represent zeros.
--
-- /n/ is the number of non-zero byte /regions/ contained in the BLOB. /m/ is
-- the number of /bytes/ in the range.
lookupRange :: Offset -> Length -> SparseBlob -> ByteString
lookupRange off len =
  BS.concat . fillZeros (off, len) . lookupRegions off len
  where
  -- Precondition: All byte regions are contained in the outer range.
  fillZeros :: (Offset, Length) -> [(Offset, ByteString)] -> [ByteString]
  fillZeros (off, len) [] = [BS.replicate len 0]
  fillZeros (off, len) ((off2, xs):ys) =
    let numZeros  = off2 - off
        xsLen     = BS.length xs
    in
    if numZeros == 0 then
      xs : fillZeros (off + xsLen, len - xsLen) ys
    else
      BS.replicate numZeros 0 : fillZeros (off2, len - numZeros) ((off2, xs):ys)

-- | /O(n)/. Returns all contained byte regions.
--
-- /n/ is the number of non-zero byte /regions/ contained in the BLOB.
allRegions :: SparseBlob -> [(Offset, ByteString)]
allRegions b = map unEntry $ allEntries b []


-- # Conversion

-- | /O(n)/. Returns all contained byte regions. Alias of /allRegions/.
--
-- /n/ is the number of non-zero byte /regions/ contained in the BLOB.
toList :: SparseBlob -> [(Offset, ByteString)]
toList = allRegions

-- | /O(n)/. Returns the BLOB containing the byte ranges.
--
-- /n/ is the number of non-zero byte /regions/ contained in the inserted
-- regions.
fromList :: [(Offset, ByteString)] -> SparseBlob
-- Specifically use `foldl'`, which ensures left-first insertion.
fromList = foldl' (\b (off, xs) -> insertRange off xs b) empty


-- # Internal Helpers #

-- ## AVL Stuff ##

-- | Removes and returns the minimum element from the the search tree.
extractMin :: SparseBlob -> Maybe (Entry, SparseBlob)
extractMin BlobNil = Nothing
extractMin (BlobNode _ _ l e r) =
  case extractMin l of
    Nothing -> return (e, r) -- The left is empty
    Just (leftE, l') -> Just (leftE, buildBalanced l' e r)

-- | Removes and returns the maximum element from the the search tree.
extractMax :: SparseBlob -> Maybe (SparseBlob, Entry)
extractMax BlobNil = Nothing
extractMax (BlobNode _ _ l e r) =
  case extractMax r of
    Nothing -> return (l, e) -- The right is empty
    Just (r', rightE) -> Just (buildBalanced l e r', rightE)

-- | Builds a tree from a left and right node when a middle entry is not
-- available.
--
-- Precondition: The first tree is /left/ of the second.
rebuildNode :: SparseBlob -> SparseBlob -> SparseBlob
rebuildNode l r =
  case extractMin r of
    Nothing -> l
    Just (rMin, r') -> buildBalanced l rMin r'

-- | Builds a /balanced/ tree.
--
-- Precondition: The first tree is left of the entry, which is left of the
--   second tree.
buildBalanced :: SparseBlob -> Entry -> SparseBlob -> SparseBlob
buildBalanced l e r =
  let balanceFactor = height r - height l
  in
  if abs balanceFactor <= 1 then
    buildSimpleNode l e r
  else if height l < height r then -- right is heavy. rotate left
    let (BlobNode rh _ rl re rr) = r
    in buildBalanced (buildBalanced l e rl) re rr
  else -- left is heavy. rotate right
    let (BlobNode _ _ ll le lr) = l
    in buildSimpleNode ll le (buildSimpleNode lr e r)
  where
  -- | Constructs a tree which only ensures the height and range are correct.
  -- Balancing must be externally ensured.
  buildSimpleNode :: SparseBlob -> Entry -> SparseBlob -> SparseBlob
  buildSimpleNode l e r =
    let h  = max (height l) (height r) + 1
        ol = foldr (maybeBinR R.union) (entryRange e) [blobRange l, blobRange r]
    in BlobNode h ol l e r


-- | /O(n)/ in the number of nodes. Returns all entries from the BLOB tree.
-- Postorder traversal with accumulator.
allEntries :: SparseBlob -> [Entry] -> [Entry]
allEntries BlobNil = id
allEntries (BlobNode _ _ l e r) =
  allEntries l . (e:) . allEntries r


-- ## Other Stuff ##

-- | Internal. Inserts the entries within the range into the blob. Segments of
-- the outer range that are not covered by entries are /zeros/.
--
-- Precondition: The entries are disjoint and sorted. The entries are contained
--   within the given range.
overwriteRange :: (Offset, Length)
               -> [Entry]
               -> SparseBlob
               -> SparseBlob
overwriteRange (off, len) newEntries b =
  let (oldEntries, b') = extractEntries (off-1, len+2) b
      newEntries'      = overwriteEntries (off, len) newEntries oldEntries
  in foldr insertEntryClean b' newEntries'

-- | Internal. /Removes/ and returns the entries that /overlap/ with the given
-- range. As entire overlapped regions are returned, the entry bounds may exceed
-- the total bound.
extractEntries :: (Offset, Length) -> SparseBlob -> ([Entry], SparseBlob)
extractEntries (off, len) b = extractEntries' b []
  where
  -- | Post-order traversal with accumulator
  extractEntries' :: SparseBlob -> [Entry] -> ([Entry], SparseBlob)
  extractEntries' b@BlobNil acc = (acc, BlobNil)
  extractEntries' b@(BlobNode _ (o,s) l e r) acc =
    if not $ R.overlaps (off, len) (o,s) then
      (acc, b)
    else
      let (acc', r') = extractEntries' r acc
      in
      if R.overlaps (off, len) (entryRange e) then
        -- Extract the node's entry
        let (acc'', l') = extractEntries' l (e : acc')
        in (acc'', rebuildNode l' r')
      else
        -- Keep the node's entry in
        let (acc'', l') = extractEntries' l acc'
        in (acc'', buildBalanced l' e r')

-- | Overwrites the entries with the data in the unpacked entry. All other bytes
-- within the range are overwritten by zeros (i.e., omitted).
--
-- Precondition: The (non-unpacked) entries /touch/ the range. The unpacked
--   entries /are contained in/ the range. Individually, both entry lists are
--   ascending on offset and internally disjoint.
overwriteEntries :: (Offset, Length) -> [Entry] -> [Entry] -> [Entry]
overwriteEntries (off, len) xs ys =
  let ysOutside = concatMap (cutContained (off, len)) ys -- length <= 2
  in map combineEntries $ groupSliding isAdjacent $ mergeAscOn entryOff xs ysOutside
  where
  -- | Concatenates the contained entries.
  --
  -- Precondition: Consecutively, the entries are adjacent.
  combineEntries :: NonEmpty Entry -> Entry
  combineEntries (x :| []) = x
  combineEntries (x :| xs) =
    Entry (entryOff x) $ BS.concat $ map entryData (x:xs)
  entryData :: Entry -> ByteString
  entryData (Entry _ xs) = xs
  -- | Returns `True` iff the entries are adjacent.
  isAdjacent :: Entry -> Entry -> Bool
  isAdjacent a b = entryEnd a == entryOff b

-- | Internal. Insert the entries into the AVL tree.
--
-- Precondition: The blob contains no entry that touches the inserted entry.
insertEntryClean :: Entry -> SparseBlob -> SparseBlob
insertEntryClean newEntry BlobNil =
  BlobNode 1 (entryRange newEntry) BlobNil newEntry BlobNil
insertEntryClean newEntry (BlobNode _ _ l e r)
  | entryEnd newEntry < entryOff e  =
      buildBalanced (insertEntryClean newEntry l) e r
  | entryOff newEntry > entryEnd e  =
      buildBalanced l e (insertEntryClean newEntry r)
  | otherwise = error "Precondition violated"

unEntry :: Entry -> (Offset, ByteString)
unEntry (Entry off xs) = (off, xs)

-- | /O(1)/. Returns the range of bytes covered by the BLOB.
blobRange :: SparseBlob -> Maybe (Offset, Length)
blobRange BlobNil = Nothing
blobRange (BlobNode _ ol _ _ _) = Just ol

-- | Bytes outside the given range are cut off.
--
-- Examples:
-- >>> cutOverflow (3, 5) (Entry 2 $ BS.pack [1,42,75,99,32,2,6,54])
-- Just (Entry 3 [42,75,99,32,2])
--
-- >>> cutOverflow (1,10) (Entry 2 $ BS.pack [1,42,75,99,32,2,6,54])
-- Just (Entry 2 [1,42,75,99,32,2,6,54])
--
-- >>> cutOverflow (11,2) (Entry 2 $ BS.pack [1,42,75,99,32,2,6,54])
-- Nothing
cutOverflow :: (Offset, Length) -> Entry -> Maybe Entry
cutOverflow (off, len) e@(Entry eOff xs) =
  do
    (eOff', len') <- R.intersect (off, len) (entryRange e)
    Just $ Entry eOff' $ byteSubregion (eOff' - eOff, len') xs

-- | Bytes inside the range are cut off.
--
-- Examples:
-- >>> cutContained (3,5) (Entry 2 $ BS.pack [1,42,75,99,32,2,6,54])
-- [Entry 2 [1],Entry 8 [6,54]]
--
-- >>> cutContained (1,5) (Entry 2 $ BS.pack [1,42,75,99,32,2,6,54])
-- [Entry 6 [32,2,6,54]]
--
-- >>> cutContained (1,10) (Entry 2 $ BS.pack [1,42,75,99,32,2,6,54])
-- []
cutContained :: (Offset, Length) -> Entry -> [Entry]
cutContained (off, len) e@(Entry eoff xs) =
  [Entry eoff $ BS.take (off - eoff) xs | eoff < off]
  ++
  [Entry (off + len) $ BS.drop (off + len - eoff) xs | entryEnd e > off + len]

-- | Returns the range of bytes covered by the entry.
entryRange :: Entry -> (Offset, Length)
entryRange (Entry o xs) = (o, BS.length xs)

entryOff :: Entry -> Offset
entryOff = fst . entryRange

entryLen :: Entry -> Length
entryLen = snd . entryRange

entryEnd :: Entry -> Offset
entryEnd e = entryOff e + entryLen e

-- | Returns the balance factor of the AVL tree.
--
-- The balance factor is the height difference between the sub-trees. As an
-- AVL-tree is balanced, this difference is one of @{-1,0,1}@.
balanceFactor :: SparseBlob -> Int
balanceFactor BlobNil = 0
balanceFactor (BlobNode _ _ l _ r) = height r - height l

-- | Returns the height of the AVL tree.
height :: SparseBlob -> Height
height BlobNil = 0
height (BlobNode h _ _ _ _) = h

-- | /O(1)/. Returns the byte at the index in the `ByteString`, if it exists.
byteAtIndex :: Int -> ByteString -> Maybe Word8
byteAtIndex = fmap fst .: BS.uncons .: BS.drop

-- | /O(1)/. Extracts a subregion within the `ByteString`.
--
-- Example:
-- >>> BS.unpack $ subregion (1,2) $ BS.pack [1, 5, 8, 42, 32, 99]
-- [5,8]
byteSubregion :: (Offset, Length) -> ByteString -> ByteString
byteSubregion (off, len) = BS.take len . BS.drop off


-- # Instances #

instance Show SparseBlob where
  showsPrec d x = showParen (d > 10)
    $ showString "fromList ["
    . intercalateShow (showString ",") (map showsEntry $ allEntries x [])
    . showString "]"
    where
    showsEntry :: Entry -> ShowS
    showsEntry (Entry off xs) = shows (off, BS.unpack xs)

instance Read SparseBlob where
  readsPrec d = readParen (d > 10) $
    \r ->
      do
        ("fromList", s) <- lex r
        (xs, s') <- reads s
        return (fromList $ map (mapSnd BS.pack) xs, s')

instance Eq SparseBlob where
  (==) a b =
    let aRegions = allRegions a
        bRegions = allRegions b
    -- First check lengths, as that is much cheaper than checking contents.
    in eqLen aRegions bRegions && aRegions == bRegions

instance Show Entry where
  showsPrec d (Entry off xs) =
    showParen (d > 10) $ showString "Entry " . shows off . showString " " . shows (BS.unpack xs)
