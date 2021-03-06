{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Probe.FFProbe (
         probeInstalled
        ,probeFile
        ,getDimensions
        ,getDuration
        ,getMediaType
        ,MediaInfo
        ,Mediatype(..)
    ) where

import Data.Eq (Eq)
import Data.List.NonEmpty (NonEmpty)
import Data.List (drop, head, foldl, take, (++))
import Data.List.Split (endBy, splitOn)
import Data.Int (Int)
import Data.Maybe
import Data.String (String)
import Data.Bool (Bool)
import Prelude (last, length, null, otherwise, read, return, (==), (||), ($), (/=), (*), (+))
import System.Directory ( getHomeDirectory )
import System.Exit ( ExitCode(..) )
import System.FilePath (FilePath)
import System.IO (IO, putStrLn)
import System.Process ( readProcessWithExitCode )
import Text.Show (Show)
import qualified Text.Regex.TDFA as RE

--------------------------------------------------------------------------------------------------
ffprobe :: String
ffprobe = "ffprobe"

--------------------------------------------------------------------------------------------------
data Mediatype = MediaAudio
                |MediaVideo
                |MediaUnknow
                deriving (Show, Eq)

data MediaInfo = MediaInfo {
         mediaType ::Mediatype
        ,duration ::Int
        ,dimensions :: (Int,Int)
    } deriving (Show, Eq)

--------------------------------------------------------------------------------------------------
getDimensions :: MediaInfo -> (Int, Int)
getDimensions (MediaInfo _  _ dim) = dim

--------------------------------------------------------------------------------------------------
getDuration :: MediaInfo -> Int
getDuration (MediaInfo _  dur _) = dur

--------------------------------------------------------------------------------------------------
getMediaType :: MediaInfo -> Mediatype
getMediaType (MediaInfo mtype  _ _) = mtype

--------------------------------------------------------------------------------------------------
ffprobeExec :: String -> IO (Maybe String)
ffprobeExec filepath = do
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode (ffprobe) ["-i", filepath] ""
    if exitCode /= ExitSuccess
        then return Nothing
        else return $ Just (stdOut ++ "\n" ++ stdErr)

--------------------------------------------------------------------------------------------------
regexCaptureAll :: String -> String -> [String]
regexCaptureAll regex text =
    if captured /= []
        then captured
        else [""]
    where
       (_, _, _, captured) = text RE.=~ regex :: (String, String, String, [String])

regexCapture :: String -> String -> String
regexCapture regex text = head $ regexCaptureAll regex text

--------------------------------------------------------------------------------------------------
dimensionsRegex = regexCapture "([0-9]{3,5}x[0-9]{3,5})"

--------------------------------------------------------------------------------------------------
durationRegex = regexCaptureAll "Duration:[ ]+([0-9]{2}):([0-9]{2}):([0-9]{2})"

--------------------------------------------------------------------------------------------------
parseDuration :: String -> Int
parseDuration text =
    (if (length timeList) == 3
        then ((read $ head timeList) * 3600)
                + ((read $ head $ drop 1 timeList) * 60)
                + ((read $ head $ drop 2 timeList))
        else 0
    )
    where
        timeList = durationRegex text

--------------------------------------------------------------------------------------------------
parseDimensions :: Int -> Int -> String -> (Int, Int)
parseDimensions width height text =
    if null dims
        then ( width, height )
        else ( head dims, last dims )
    where
        dims = [ read x ::Int | x <- splitOn "x" $ dimensionsRegex text, x /= ""]

--------------------------------------------------------------------------------------------------
hasVideo :: String -> Bool
hasVideo text = text RE.=~ ("Video: " ::String)

--------------------------------------------------------------------------------------------------
hasAudio :: String -> Bool
hasAudio text = text RE.=~ ("Audio: " ::String)

--------------------------------------------------------------------------------------------------
parseMediaType :: Mediatype -> String -> Mediatype
parseMediaType previous text
        | previous == MediaVideo = previous
        | hasVideo text = MediaVideo
        | hasAudio text = MediaAudio
        | otherwise = previous

--------------------------------------------------------------------------------------------------
type ParserData = (Mediatype, Int, (Int, Int))
emptyParserData :: ParserData
emptyParserData = (MediaUnknow, 0, (0, 0))

--------------------------------------------------------------------------------------------------
parseNext :: ParserData  -> String -> ParserData
parseNext (mType, duration, (width, height)) text = (
         if mType /= MediaVideo
            then parseMediaType mType text
            else mType
        ,if duration == 0
            then parseDuration text
            else duration
        ,if (width == 0) || (height == 0)
            then parseDimensions width height text
            else (width, height)
    )

--------------------------------------------------------------------------------------------------
parseAll :: [String] -> ParserData
parseAll contents = foldl (\last line -> parseNext last line) emptyParserData $ contents

--------------------------------------------------------------------------------------------------
parseInfo :: [String] -> MediaInfo
parseInfo contents = MediaInfo {
         mediaType=mType
        ,duration=mDuration
        ,dimensions=mDimensions
    } where
        (mType, mDuration, mDimensions) = parseAll contents

--------------------------------------------------------------------------------------------------
probeInstalled :: IO Bool
probeInstalled = do
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode (ffprobe) ["-version"] ""
    return $ exitCode == ExitSuccess

--------------------------------------------------------------------------------------------------
probeFile :: FilePath -> IO (Maybe MediaInfo)
probeFile filepath = do
    probedData <- ffprobeExec filepath
    case probedData of
        Just info -> return $ Just $ parseInfo $ endBy "\n" info
        Nothing -> return Nothing
