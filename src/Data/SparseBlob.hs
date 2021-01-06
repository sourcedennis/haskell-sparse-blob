
--------------------------------------------------------------------------------
-- |
-- Module      : Data.SparseBlob
-- Copyright   : (c) Dennis Sprokholt, 2021
-- License     : BSD-3-Clause
--
-- Maintainer  : me@dennis.life
-- Stability   : experimental
-- Portability : portable
--
-- = Description
--
-- An efficient implementation of a sparse Binary Large OBject (BLOB), which
-- effectively is a vector of bytes. A BLOB is /sparse/ when it contains mainly
-- zero-values, with occasional non-zero regions. This implementation stores
-- consecutive regions of non-zero bytes as nodes in an AVL tree. Zeros are
-- /not/ explicitly stored in the tree.
--
-- = Time Complexity
--
-- The /average-case time complexity/ of most operations is /O(log n)/, as
-- dominated by operations on the AVL tree. However, on some pathological
-- inputs - i.e., a large vector /without/ zero bytes - the worst-case time
-- complexity of operations is /O(n)/, as dominated by
-- `Data.ByteString.ByteString` operations. For all functions, their
-- /average-case time complexity/ (over all possible inputs) is thus given.
--------------------------------------------------------------------------------
module Data.SparseBlob
  ( -- * Data Structures
    SparseBlob
    -- * Type Aliases
  , Offset
  , Length
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
-- Local imports
import Data.SparseBlob.Internal
import Data.SparseBlob.Range ( Offset, Length )
