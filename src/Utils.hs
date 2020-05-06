{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils (
     baseDir
    ,baseFilePath
    ,readContents
    ,writeContents
    ,getModifiedTime
    ) where

import Data.String (String)
import Data.Int (Int)
import Prelude (return, properFraction, ($), (*), (/), floor, div)
import System.Directory (doesFileExist, getModificationTime)
import System.Environment (getExecutablePath)
import System.FilePath (combine, dropFileName, FilePath)
import System.IO (IO, readFile, writeFile)
import Data.Time.Clock (UTCTime(..), nominalDiffTimeToSeconds)
import Data.Time.LocalTime (utcToLocalTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

---------------------------------------------------------------------------------------------------
baseDir :: IO String
baseDir = do
    path <- getExecutablePath
    return $ dropFileName path

---------------------------------------------------------------------------------------------------
baseFilePath :: String -> IO String
baseFilePath path = do
    dir <- baseDir
    return $ combine dir path

---------------------------------------------------------------------------------------------------
readContents :: String -> IO String
readContents filename = do
    filepath <- baseFilePath filename
    fileexists <- doesFileExist filepath
    if fileexists
        then readFile filepath
        else return ("{}" ::String)

---------------------------------------------------------------------------------------------------
writeContents :: String -> String -> IO ()
writeContents filename contents = do
    filepath <- baseFilePath filename
    writeFile filepath contents

---------------------------------------------------------------------------------------------------
secsSinceEpoch :: UTCTime -> Int
secsSinceEpoch time =
    (floor $ (1e9 *) $ nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds time) `div` 1000000000

---------------------------------------------------------------------------------------------------
getModifiedTime :: FilePath -> IO Int
getModifiedTime path = do
    modified <- getModificationTime path
    return $ secsSinceEpoch $ modified
