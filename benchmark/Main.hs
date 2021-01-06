-- Some simple benchmarks comparing the `SparseBlob` implementation against a
-- much simpler `SimpleBlob` (`Data.IntMap`) implementation .
--
-- TODO: Add more

-- Stdlib imports
import           Data.Word ( Word8 )
-- Extra stdlib imports
import qualified Data.ByteString as BS
import           Data.ByteString ( ByteString )
-- External library imports
import           Criterion.Main
-- Local imports
import qualified Data.SparseBlob ( SparseBlob )
import           Data.SparseBlob as SB
import qualified Data.SparseBlob.SimpleBlob ( SimpleBlob )
import           Data.SparseBlob.SimpleBlob as SI

main =
  let memory  = [g 0 300, g 340 200, g 600 32, g 1000 56, g 9999 3452]
      offsets = [4,8,9,11,45,50,70,72,74,99,120,124,167,603,630,1003,1005,9999,10302]
      ranges  = [(200,400),(700,500),(9000,2500)]
      sparse  = SB.fromList memory
      simple  = SI.fromList memory
  in
  defaultMain
    [ bgroup "lookup"
        [ bench "sparse"  $ whnf (map (`SB.lookup` sparse)) offsets
        , bench "simple"  $ whnf (map (`SI.lookup` simple)) offsets
        ]
    , bgroup "delete"
        [ bench "sparse"  $ whnf (foldr (uncurry SB.deleteRange) sparse) ranges
        , bench "simple"  $ whnf (foldr (uncurry SI.deleteRange) simple) ranges
        ]
    ]

g :: Offset -> Length -> (Offset, ByteString)
g off len = (off, BS.replicate len 42)
