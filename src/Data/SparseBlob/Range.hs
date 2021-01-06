
-- | Operations for integer ranges, which are specified by their offset and
-- length. Conceptually, these represent the: [offset, offset+length). The left
-- bound is /inclusive/ while the right bound is /exclusive/. So, the range
-- [4,9) contains elements [4,5,6,7,8]. This range is represented with
-- @offset=4@ and @length=5@.
--
-- WARNING: The defined functions are only expected to "behave well" for
--   non-negative lengths.
module Data.SparseBlob.Range
  ( -- * Type Aliases
    Offset
  , Length
    -- * Functions
  , member
  , union
  , intersect
  , isSubrange
  , overlaps
  ) where


-- # Type Aliases #

-- | Offset in a BLOB
type Offset = Int

-- | Number of bytes in a BLOB. Must be non-negative.
type Length = Int


-- # Functions #

-- | Returns `True` iff the offset is contained within the range.
member :: Offset -> (Offset, Length) -> Bool
member i (off, len) = off <= i && i < off + len

-- | Returns the smallest range containing both input ranges.
--
-- Examples:
-- >>> union (1,3) (5, 3)
-- (1,7)
union :: (Offset, Length) -> (Offset, Length) -> (Offset, Length)
union (o1, l1) (o2, l2) =
  let o = min o1 o2
  in (o, max (o1 + l1) (o2 + l2) - o)

-- | Returns the largest range contained in both input ranges. If no such range
-- exist (i.e., it has length 0), then `Nothing` is returned.
--
-- Examples:
-- >>> intersect (1,3) (2,3)
-- Just (2,2)
--
-- >>> intersect (1,2) (3,2)
-- Nothing
intersect :: (Offset, Length) -> (Offset, Length) -> Maybe (Offset, Length)
intersect (o1, l1) (o2, l2) =
  let o = max o1 o2
      l = min (o1 + l1) (o2 + l2) - o
  in
  if l > 0 then
    Just (o, l)
  else
    Nothing

-- | Returns `True` iff the first range is contained in the second range.
--
-- Range @A@ is contained in range @B@ if every location in @A@ is also
-- contained in range @B@.
--
-- Examples:
-- >>> isSubrange (1,3) (0,4)
-- True
--
-- >>> isSubrange (1,3) (0,3)
-- False
--
-- Example, which is vacuously true, as the first range contains no elements:
-- >>> isSubrange (42, 0) (2,3)
-- True
isSubrange :: (Offset, Length) -> (Offset, Length) -> Bool
isSubrange (_, 0) _ = True -- Containment vacuously true
isSubrange (o1, l1) (o2, l2) = o1 >= o2 && o1 + l1 <= o2 + l2

-- | Returns `True` iff there exists a location which is contained in both
-- regions. Commutative.
--
-- Examples:
-- >>> overlaps (1,3) (3,2)
-- True
--
-- >>> overlaps (1,2) (3,2)
-- False
--
-- >>> overlaps (1,0) (0,4)
-- False
overlaps :: (Offset, Length) -> (Offset, Length) -> Bool
overlaps (_, 0) _ = False
overlaps _ (_, 0) = False
overlaps (o1, l1) (o2, l2) = o1 < o2 + l2 && o2 < o1 + l1
