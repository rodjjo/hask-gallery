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
import Data.List (head, foldl, (++))
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
        ,duration ::String
        ,dimensions :: (Int,Int)
    } deriving (Show, Eq)

--------------------------------------------------------------------------------------------------
ffprobeExec :: String -> IO (Maybe String)
ffprobeExec filepath = do
    (exitCode, stdOut, stdErr) <- readProcessWithExitCode (ffprobe) ["-i", filepath] ""
    if exitCode /= ExitSuccess
        then return Nothing
        else return $ Just (stdOut ++ "\n" ++ stdErr)

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
type ParserData = (Mediatype, String, (Int, Int))
emptyParserData :: ParserData
emptyParserData = (MediaUnknow, "", (0, 0))

--------------------------------------------------------------------------------------------------
parseNext :: ParserData  -> String -> ParserData
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
