module Stream.File (
        streamFile
    ) where

import qualified Data.ByteString.Lazy as L
import Data.Int (Int)
import Data.ByteString.Builder
import System.FilePath (FilePath)

--------------------------------------------------------------------------------------------------
streamFile :: FilePath -> Int -> IO L.ByteString
streamFile filePath seekPosition = L.readFile filePath
