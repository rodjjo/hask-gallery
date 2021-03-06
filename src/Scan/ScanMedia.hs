{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Scan.ScanMedia (
         searchForVideos
        ,searchForPictures
        ,searchForMusics
    ) where

import Utils (lowerPath)
import Control.Monad (mapM, filterM)
import Data.Bool (Bool(..))
import Data.Int (Int)
import Data.List ((++), elem, foldl, map)
import Data.String (String)
import Data.Char (toLower)
import Prelude (not, return, (==), ($))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropFileName, FilePath, takeExtension)
import System.FilePath.Posix ((</>))
import System.IO (IO)

---------------------------------------------------------------------------------------------------
anyOfExtension :: [String] -> String -> Bool
anyOfExtension filter filepath = elem (takeExtension $ lowerPath filepath) filter

---------------------------------------------------------------------------------------------------
mp4Filter = anyOfExtension [".mp4"] -- the most supported extension for browsers

---------------------------------------------------------------------------------------------------
pictureFilter = anyOfExtension [".jpg", ".jpeg", ".png", ".webp", ".bmp"]

---------------------------------------------------------------------------------------------------
musicFilter = anyOfExtension [".mp3", ".ogg", ".wav"]

---------------------------------------------------------------------------------------------------
dirTree :: FilePath -> IO [FilePath]
dirTree directorypath = do
    exists <- doesDirectoryExist directorypath
    contents <- (if exists
                then listDirectory directorypath
                else return [])
    contentsin <- (if exists
                    then mapM dirTree [ directorypath </> p | p <- contents ]
                    else return [[]])
    return $ [ directorypath </> p | p <- contents ] ++ foldl (\i l -> i ++ l) [] contentsin

---------------------------------------------------------------------------------------------------
allFilesInSubDirs :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
allFilesInSubDirs filter directorypath = do
    allContents <- dirTree directorypath
    filterM doesFileExist [path | path <- allContents, filter path]

---------------------------------------------------------------------------------------------------
searchFilesFilter :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
searchFilesFilter filter = allFilesInSubDirs filter

---------------------------------------------------------------------------------------------------
searchForVideos = searchFilesFilter mp4Filter
searchForPictures = searchFilesFilter pictureFilter
searchForMusics = searchFilesFilter musicFilter
