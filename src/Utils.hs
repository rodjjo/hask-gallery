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
    ) where

import Data.String (String)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath (combine, dropFileName, FilePath)
import System.IO (IO, readFile, writeFile)
import Prelude (return, ($))

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
