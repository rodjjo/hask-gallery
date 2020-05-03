{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Models.Settings (
         new
        ,save
        ,load
        ,setTitle
        ,getTitle
        ,setVideoGalleryPath
        ,getVideoGalleryPath
        ,optionSetter
    ) where

import Models.Base (
         loadModel
        ,saveModel
        ,saveModelList
        ,loadModelList
    )

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Int (Int)
import Data.String (String)
import GHC.Generics (Generic)
import Prelude (return, ($), (==), otherwise)
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
new :: Settings
new = Settings { title="Hask Gallery",  video_gallery_path="" }


----------------------------------------------------------------------------------------------------
optionSetter :: String -> (Settings -> String -> Settings)
optionSetter name
    | (name == "title") = setTitle
    | (name == "videos-path") = setVideoGalleryPath
    | otherwise = (\settings text -> settings)

----------------------------------------------------------------------------------------------------
setTitle :: Settings -> String -> Settings
setTitle settings value = Settings { title=value, video_gallery_path=getVideoGalleryPath settings }

----------------------------------------------------------------------------------------------------
getTitle :: Settings -> String
getTitle (Settings title _) = title

----------------------------------------------------------------------------------------------------
setVideoGalleryPath :: Settings -> String -> Settings
setVideoGalleryPath settings value = Settings { title=getTitle settings, video_gallery_path=value }

----------------------------------------------------------------------------------------------------
getVideoGalleryPath :: Settings -> String
getVideoGalleryPath (Settings _ path) = path

----------------------------------------------------------------------------------------------------
filename = "gallery-settings.json"

---------------------------------------------------------------------------------------------------
load :: IO Settings
load = loadModel filename new

---------------------------------------------------------------------------------------------------
save :: Settings -> IO ()
save config = saveModel filename config
