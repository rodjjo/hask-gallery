module Stream.File (
        streamFile
    ) where

import qualified Data.ByteString.Lazy as L
import Data.Int (Int)
import System.FilePath (FilePath)
import System.IO (IO, withBinaryFile, hFileSize, hSeek, IOMode(ReadMode), SeekMode(AbsoluteSeek))
import Prelude (fromInteger, toInteger, (<), (>=), (<=), (&&), (-), ($))

--------------------------------------------------------------------------------------------------
streamFile :: FilePath -> Int -> Int -> IO L.ByteString
streamFile filePath seekPosition bytes2Read =
    withBinaryFile  filePath ReadMode $ \h -> do
        fileSizeI <- hFileSize h
        let fileSize = (fromInteger fileSizeI) ::Int
        let seekPos = (if seekPosition <= fileSize then seekPosition else fileSize)
        hSeek h AbsoluteSeek $ toInteger seekPos
        let filesz = (fileSize - seekPos)
        let limit = (if ((bytes2Read >= 0) && (bytes2Read <= filesz)) then bytes2Read else filesz)
        L.hGet h limit
