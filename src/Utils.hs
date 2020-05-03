{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Utils (
     baseDir
    ,baseFilePath
    ,loadModel
    ,loadModelList
    ,saveModel
    ,saveModelList
    ) where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe
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

---------------------------------------------------------------------------------------------------
loadModel :: (FromJSON a) => String -> a -> IO a
loadModel filename def = do
    contents <- readContents filename
    case (decode (Char8.pack contents)) of
        Just a -> return a
        Nothing -> return def

---------------------------------------------------------------------------------------------------
loadModelList :: (FromJSON a) => String -> IO [a]
loadModelList filename = do
    contents <- readContents filename
    case (decode (Char8.pack contents)) of
        Just a -> return a
        Nothing -> return []

---------------------------------------------------------------------------------------------------
saveModel :: (ToJSON a) => String -> a -> IO ()
saveModel filename value = do
    writeContents filename $ Char8.unpack $ encode value

---------------------------------------------------------------------------------------------------
saveModelList :: (ToJSON a) => String -> [a] -> IO ()
saveModelList filename value = do
    writeContents filename $ Char8.unpack $ encode value
