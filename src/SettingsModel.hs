{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module SettingsModel (
        save,
        load,
        getTitle,
        getVideoGalleryPath,
        Settings(..),
    ) where

import Utils (
         loadModel
        ,saveModel
        ,saveModelList
        ,loadModelList
    )

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Int (Int)
import Data.String (String)
import GHC.Generics (Generic)
import Prelude (return, ($))
import System.IO (IO)
import Text.Show (Show)

---------------------------------------------------------------------------------------------------
data Settings = Settings {
        title ::String,
        video_gallery_path ::String
    } deriving (Show, Generic)
instance FromJSON Settings
instance ToJSON Settings

----------------------------------------------------------------------------------------------------
getTitle :: Settings -> String
getTitle (Settings title _) = title
getVideoGalleryPath :: Settings -> String
getVideoGalleryPath (Settings _ path) = path
configurationFile = "gallery-settings.json"

---------------------------------------------------------------------------------------------------
load :: IO Settings
load = loadModel configurationFile (Settings "Hask Gallery" "")

---------------------------------------------------------------------------------------------------
save :: Settings -> IO ()
save config = saveModel configurationFile config
