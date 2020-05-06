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
        ,quicksort
        ,quicksortM
    ) where

import Control.Monad (Monad)
import Data.String (String)
import Data.Int (Int)
import Data.Ord (Ord)
import Data.List (filter, (++))
import Prelude (return, floor, div, ($), (*), (/), (<), (>=))
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
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

---------------------------------------------------------------------------------------------------
quicksortM :: (Ord t, Monad m) => m [t]  -> m [t]
quicksortM mylist = do
    list <- mylist
    return $ quicksort list

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
