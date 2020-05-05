{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Probe.FFProbe (
         probeInstalled
        ,probeFile
        --,getDimensions
        --,getDuration
        --,getMediaType
        --,getModifiedAt
        --,getCreatedAt
        ,Mediatype(..)
    ) where

import Data.Eq (Eq)
import Data.List.NonEmpty (NonEmpty)
import Data.List (head, foldl)
import Data.List.Split (endBy, splitOn)
import Data.Int (Int)
import Data.Maybe
import Data.String (String)
import Data.Bool (Bool)
import Prelude (last, null, otherwise, read, return, (==), (||), ($), (/=))
import System.Directory ( getHomeDirectory )
import System.Exit ( ExitCode(..) )
import System.FilePath (FilePath)
import System.IO (IO, putStrLn)
import System.Process ( readProcessWithExitCode )
import qualified Text.Regex.TDFA as RE

--------------------------------------------------------------------------------------------------
ffprobe :: String
ffprobe = "ffprobe"

--------------------------------------------------------------------------------------------------
data Mediatype = MediaAudio|MediaVideo|MediaUnknow deriving Eq

data MediaInfo = MediaInfo {
         mediaType ::Mediatype
        ,duration ::String
        ,modifiedAt ::Int
        ,createdAt ::Int
        ,dimensions :: (Int,Int)
    }

--------------------------------------------------------------------------------------------------
ffprobeExec :: String -> IO (Maybe String)
ffprobeExec filepath = do
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode (ffprobe) ["-i", filepath] ""
    if exitCode /= ExitSuccess
        then return Nothing
        else return $ Just stdOut

--------------------------------------------------------------------------------------------------
regexCapture :: String -> String -> String
regexCapture regex text =
    if captured /= []
        then head captured
        else ""
    where
       (_, _, _, captured) = text RE.=~ regex :: (String, String, String, [String])

--------------------------------------------------------------------------------------------------
dimensionsRegex = regexCapture "([0-9]{3,5}x[0-9]{3,5})"

--------------------------------------------------------------------------------------------------
durationRegex = regexCapture "Duration:[ ]+([0-9]{2}:[0-9]{2}:[0-9]{2})"

--------------------------------------------------------------------------------------------------
parseDuration :: String -> String
parseDuration text = durationRegex text

--------------------------------------------------------------------------------------------------
parseDimensions :: Int -> Int -> String -> (Int, Int)
parseDimensions width height text =
    if null dims
        then ( width, height )
        else ( head dims, last dims )
    where
        dims = [ read x ::Int | x <- splitOn "x" $ dimensionsRegex text ]

--------------------------------------------------------------------------------------------------
parseMediaType :: Mediatype -> String -> Mediatype
parseMediaType mediaType text = mediaType

--------------------------------------------------------------------------------------------------
type ParseData = (Mediatype, String, (Int, Int))
emptyParseData :: ParseData
emptyParseData = (MediaUnknow, "", (0, 0))

--------------------------------------------------------------------------------------------------
parseNext :: ParseData  -> String -> ParseData
parseNext (mType, duration, (width, height)) text = (
         if mType /= MediaVideo
            then parseMediaType mType text
            else mType
        ,if duration == ""
            then parseDuration text
            else duration
        ,if (width == 0) || (height == 0)
            then parseDimensions width height text
            else (width, height)
    )

--------------------------------------------------------------------------------------------------
parseAll :: [String] -> (Mediatype, String, (Int, Int))
parseAll contents = foldl (\last line -> parseNext last line) emptyParseData $ contents

--------------------------------------------------------------------------------------------------
parseInfo :: Int -> Int -> [String] -> MediaInfo
parseInfo mCreatedAt mModifiedAt contents = MediaInfo {
         mediaType=mType
        ,duration=mDuration
        ,modifiedAt=mModifiedAt
        ,createdAt=mCreatedAt
        ,dimensions=mDimensions
    } where
        (mType, mDuration, mDimensions) = parseAll contents

--------------------------------------------------------------------------------------------------
probeInstalled :: IO Bool
probeInstalled = do
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode (ffprobe) ["-version"] ""
    return $ exitCode == ExitSuccess

--------------------------------------------------------------------------------------------------
getModificationTime :: FilePath -> IO Int
getModificationTime filepath = do
    return 0

--------------------------------------------------------------------------------------------------
getCreationTime :: FilePath -> IO Int
getCreationTime filepath = do
    return 0

--------------------------------------------------------------------------------------------------
probeFile :: FilePath -> IO (Maybe MediaInfo)
probeFile filepath = do
    probedData <- ffprobeExec filepath
    mCreatedAt <- getCreationTime filepath
    mModifiedAt <- getModificationTime filepath
    case probedData of
        Just info -> return $ Just $ parseInfo mCreatedAt mModifiedAt $ endBy "\n" info
        Nothing -> return Nothing
