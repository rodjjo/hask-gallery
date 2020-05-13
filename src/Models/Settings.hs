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
        ,setMusicGalleryPath
        ,getMusicGalleryPath
        ,setPictureGalleryPath
        ,getPictureGalleryPath
        ,optionSetter
        ,validOptions
    ) where

import Models.Base (
         loadModel
        ,saveModel
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
     title ::String
    ,video_gallery_path ::String
    ,picture_gallery_path ::String
    ,music_gallery_path ::String
    } deriving (Show, Generic)
instance FromJSON Settings
instance ToJSON Settings

----------------------------------------------------------------------------------------------------
new :: Settings
new = Settings {
     title="Hask Gallery"
    ,video_gallery_path=""
    ,picture_gallery_path=""
    ,music_gallery_path=""
    }

----------------------------------------------------------------------------------------------------
validOptions = ["title", "videos-path", "musics-path", "pictures-path"]

----------------------------------------------------------------------------------------------------
optionSetter :: String -> (Settings -> String -> Settings)
optionSetter name
    | (name == "title") = setTitle
    | (name == "videos-path") = setVideoGalleryPath
    | (name == "musics-path") = setMusicGalleryPath
    | (name == "pictures-path") = setPictureGalleryPath
    | otherwise = (\settings text -> settings)

----------------------------------------------------------------------------------------------------
setTitle :: Settings -> String -> Settings
setTitle (Settings _ p2 p3 p4 ) value = Settings value p2 p3 p4

----------------------------------------------------------------------------------------------------
getTitle :: Settings -> String
getTitle (Settings title _ _ _) = title

----------------------------------------------------------------------------------------------------
setVideoGalleryPath :: Settings -> String -> Settings
setVideoGalleryPath (Settings p1 _ p3 p4 ) value = Settings  p1 value p3 p4

----------------------------------------------------------------------------------------------------
getVideoGalleryPath :: Settings -> String
getVideoGalleryPath (Settings _ path _ _) = path

----------------------------------------------------------------------------------------------------
setMusicGalleryPath :: Settings -> String -> Settings
setMusicGalleryPath (Settings p1 p2 p3 _) value = Settings p1 p2 p3 value

----------------------------------------------------------------------------------------------------
getMusicGalleryPath :: Settings -> String
getMusicGalleryPath (Settings _ _ _ path) = path

----------------------------------------------------------------------------------------------------
setPictureGalleryPath :: Settings -> String -> Settings
setPictureGalleryPath (Settings p1 p2 _ p4) value = Settings p1 p2 value p4

----------------------------------------------------------------------------------------------------
getPictureGalleryPath :: Settings -> String
getPictureGalleryPath (Settings _ _ path _) = path

----------------------------------------------------------------------------------------------------
filename = "gallery-settings.hgl"

---------------------------------------------------------------------------------------------------
load :: IO Settings
load = loadModel filename new

---------------------------------------------------------------------------------------------------
save :: Settings -> IO ()
save config = saveModel filename config
