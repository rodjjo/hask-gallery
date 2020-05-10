module Stream.File (
         getSeekRange
        ,getFileSize
        ,streamFile
        ,RangeInfo
    ) where

import qualified Data.ByteString.Lazy as L
import Data.Int (Int)
import System.FilePath (FilePath)
import System.IO (withBinaryFile, hFileSize, hSeek, IO, Handle, IOMode(ReadMode), SeekMode(AbsoluteSeek))
import Prelude (return, fromInteger, toInteger, (<), (>=), (<=), (&&), (-), ($))

--------------------------------------------------------------------------------------------------
type RangeInfo =  (Int, Int, Int)

--------------------------------------------------------------------------------------------------
getSeekRangeH :: Handle -> Int -> Int -> IO RangeInfo
getSeekRangeH h seekPosition bytes2Read = do
        fileSizeI <- hFileSize h
        let fileSize = (fromInteger fileSizeI) ::Int
        let seekPos = (if seekPosition < fileSize then seekPosition else fileSize - 1)
        let filesz = (fileSize - seekPos)
        let limit = (if ((bytes2Read >= 0) && (bytes2Read < filesz)) then bytes2Read else filesz)
        return (seekPos, limit, fileSize)

--------------------------------------------------------------------------------------------------
getSeekRange :: FilePath -> Int -> Int -> IO RangeInfo
getSeekRange filePath seekPosition bytes2Read =
    withBinaryFile filePath ReadMode $ \h -> do
        rangeInfo <- getSeekRangeH h seekPosition bytes2Read
        return rangeInfo

--------------------------------------------------------------------------------------------------
getFileSize :: FilePath -> IO Int
getFileSize filePath =
    withBinaryFile filePath ReadMode $ \h -> do
        fileSizeI <- hFileSize h
        return $ fromInteger fileSizeI

--------------------------------------------------------------------------------------------------
streamFile :: FilePath -> Int -> Int -> IO L.ByteString
streamFile filePath seekPosition bytes2Read =
    withBinaryFile filePath ReadMode $ \h -> do
        (seekPos, limit, fileSize) <- getSeekRangeH h seekPosition bytes2Read
        hSeek h AbsoluteSeek $ toInteger seekPos
        L.hGet h limit
