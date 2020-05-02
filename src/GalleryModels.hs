{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module GalleryModels (
        getConfiguration,
        setConfiguration,
        refreshGallery,
        loadGallery,
        configurationTitle,
        configurationPath,
        ConfigurationModel(..)
    ) where

import Control.Exception.Base (try)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Either
import Data.Int (Int)
import Data.List ((++), head)
import Data.Maybe
import Data.String (String)
import Data.Text (pack, Text)
import GHC.Generics (Generic)
import Prelude (return, ($))
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath (combine, dropFileName)
import System.IO (IO, readFile, writeFile, putStrLn)
import Text.Show (Show)

data ConfigurationModel = ConfigurationModel {
        title ::String,
        path ::String
    } deriving (Show, Generic)
instance FromJSON ConfigurationModel
instance ToJSON ConfigurationModel

configurationTitle :: ConfigurationModel -> String
configurationTitle (ConfigurationModel title _) = title
configurationPath :: ConfigurationModel -> String
configurationPath (ConfigurationModel _ path) = path

configurationFile = "gallery-settings.json"

baseDir :: IO String
baseDir = do
    path <- getExecutablePath
    return $ dropFileName path

readContents :: String -> IO String
readContents filename = do
    directory <- baseDir
    let filepath = combine directory filename
    fileexists <- doesFileExist filepath
    if fileexists
        then readFile filepath
        else return ("{}" ::String)

writeContents :: String -> String -> IO ()
writeContents filename contents = do
    directory <- baseDir
    let filepath = combine directory filename
    writeFile filepath contents

getConfiguration :: IO ConfigurationModel
getConfiguration = do
    contents <- readContents configurationFile
    case (decode (Char8.pack contents) :: Maybe ConfigurationModel) of
        Just c -> return (c ::ConfigurationModel)
        Nothing -> return (ConfigurationModel "Hask Gallery" "")


setConfiguration :: ConfigurationModel -> IO ()
setConfiguration config = do
    writeContents configurationFile (Char8.unpack (encode config))


refreshGallery :: IO ()
refreshGallery = putStrLn("Not Implemented yet")

loadGallery :: IO ()
loadGallery = putStrLn("Not Implemented yet")
