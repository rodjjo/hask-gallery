{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Models.Picture (
     loadList
    ,getGalleryPictures
    ,getGalleryPath
    ,getGalleryCount
    ,updateCache
    ,Picture(..)
    ,PictureList
    ,PictureGallery
) where

import Models.Base (
         loadModel
        ,saveModel
    )

import Scan.ScanMedia (
        searchForPictures
    )

import Utils (
         getModifiedTime
        ,quicksort
        ,quicksortM
        ,dropFirstSlash
        ,pathContains
    )

import Control.Monad (mapM)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Int (Int)
import Data.List (drop)
import Data.String (String)
import Data.Eq (Eq)
import Data.Maybe (fromJust, Maybe(Nothing))
import Data.Ord (Ord)
import GHC.Generics (Generic)
import Prelude (length, return, ($), (==), otherwise)
import System.IO (IO, putStrLn)
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
picPath :: Picture -> String
picPath (Picture p _) = p

---------------------------------------------------------------------------------------------------
loadList :: IO PictureGallery
loadList = loadModel filename emptyGallery

---------------------------------------------------------------------------------------------------
getGalleryPictures :: PictureGallery -> Maybe String -> PictureList
getGalleryPictures (PictureGallery p _ _) filter = if filter == Nothing then p else [ i | i <- p, pathContains (fromJust filter) (picPath i) ]

---------------------------------------------------------------------------------------------------
getGalleryPath :: PictureGallery -> String
getGalleryPath (PictureGallery _ p _) = p

---------------------------------------------------------------------------------------------------
getGalleryCount :: PictureGallery -> Int
getGalleryCount (PictureGallery _ _ c) = c

---------------------------------------------------------------------------------------------------
newPictureModel :: String -> String -> IO Picture
newPictureModel basePath picturePath = do
    timeStamp <- getModifiedTime picturePath
    let normPath = drop (length basePath) picturePath
    return $ Picture normPath timeStamp

---------------------------------------------------------------------------------------------------
updateCache :: String -> IO ()
updateCache galleryPath = do
    putStrLn "Updating picture gallery"
    pictureList <- quicksortM $ searchForPictures galleryPath
    items <- mapM (\path -> (newPictureModel galleryPath path)) pictureList
    let gallery = PictureGallery { galleryItems=items
                                 , baseFilePath=galleryPath
                                 , totalCount=length pictureList
                                 }
    saveModel filename gallery
    putStrLn "The picture gallery was updated"
