{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Models.Music (
    loadList
    ,Music(..)
    ,MusicList
    ,MusicGallery
) where

import Models.Base (
         loadModel
        ,saveModel
    )

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Int (Int)
import Data.String (String)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Ord (Ord)
import GHC.Generics (Generic)
import Prelude (return, ($), (==), otherwise)
import System.IO (IO)
import Text.Show (Show)


---------------------------------------------------------------------------------------------------
data Music = Music { musicPath ::String
                   , modified ::Int
                   , duration ::Int
                   } deriving (Show, Generic, Eq)
instance FromJSON Music
instance ToJSON Music

type MusicList = [Music]

---------------------------------------------------------------------------------------------------
data MusicGallery = MusicGallery { galleryItems ::MusicList
                                 , baseFilePath ::String
                                 , totalCount ::Int
                                 , totalDuration ::Int
                                 } deriving (Show, Generic, Eq)
instance FromJSON MusicGallery
instance ToJSON MusicGallery

filename = "gallery-musics.hgl"

emptyGallery = MusicGallery ([] ::MusicList) "" 0 0

---------------------------------------------------------------------------------------------------
loadList :: IO MusicGallery
loadList = loadModel filename emptyGallery
