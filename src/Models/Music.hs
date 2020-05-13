{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Models.Music (
     loadList
    ,getGalleryMusics
    ,getGalleryPath
    ,getGalleryDuration
    ,getGalleryCount
    ,updateCache
    ,Music(..)
    ,MusicList
    ,MusicGallery
) where

import Scan.ScanMedia (
        searchForMusics
    )

import Models.Base (
         loadModel
        ,saveModel
    )

import Utils (
         getModifiedTime
        ,quicksort
        ,quicksortM
        ,dropFirstSlash
    )

import Control.Monad (mapM)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Int (Int)
import Data.List (drop)
import Data.String (String)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Ord (Ord)
import GHC.Generics (Generic)
import Prelude (length, return, ($), (==), otherwise)
import System.IO (IO, putStrLn)
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

---------------------------------------------------------------------------------------------------
getGalleryMusics :: MusicGallery -> MusicList
getGalleryMusics (MusicGallery m _ _ _) = m

---------------------------------------------------------------------------------------------------
getGalleryPath :: MusicGallery -> String
getGalleryPath (MusicGallery _ p _ _) = p

---------------------------------------------------------------------------------------------------
getGalleryDuration :: MusicGallery -> Int
getGalleryDuration (MusicGallery _ _ _ d) = d

---------------------------------------------------------------------------------------------------
getGalleryCount :: MusicGallery -> Int
getGalleryCount (MusicGallery _ _ c _) = c

---------------------------------------------------------------------------------------------------
newMusicModel :: String -> String -> IO Music
newMusicModel basePath picturePath = do
    timeStamp <- getModifiedTime picturePath
    let normPath = drop (length basePath) picturePath
    return $ Music normPath timeStamp 0

---------------------------------------------------------------------------------------------------
updateCache :: String -> IO ()
updateCache galleryPath = do
    putStrLn "Updating music gallery"
    pictureList <- quicksortM $ searchForMusics galleryPath
    items <- mapM (\path -> (newMusicModel galleryPath path)) pictureList
    let gallery = MusicGallery { galleryItems=items
                                 , baseFilePath=galleryPath
                                 , totalCount=length pictureList
                                 , totalDuration=0
                                 }
    saveModel filename gallery
    putStrLn "The music gallery was updated"
