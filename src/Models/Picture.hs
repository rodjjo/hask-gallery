{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Models.Picture (
     Picture(..)
    ,PictureList
    ,PictureGallery
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
data Picture = Picture { picturePath ::String
                   , modified ::Int
                   } deriving (Show, Generic, Eq)
instance FromJSON Picture
instance ToJSON Picture

type PictureList = [Picture]

---------------------------------------------------------------------------------------------------
data PictureGallery = PictureGallery { galleryItems ::PictureList
                                 , baseFilePath ::String
                                 , totalCount ::Int
                                 } deriving (Show, Generic, Eq)
instance FromJSON PictureGallery
instance ToJSON PictureGallery

filename = "gallery-pictures.hgl"

emptyGallery = PictureGallery ([] ::PictureList) "" 0

---------------------------------------------------------------------------------------------------
loadList :: IO PictureGallery
loadList = loadModel filename emptyGallery
