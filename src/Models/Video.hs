{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Models.Video (
         updateCache
        ,loadList
        ,Video(..)
    ) where

import Utils (
         loadModel
        ,saveModel
        ,saveModelList
        ,loadModelList
    )

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int)
import Data.String (String)
import System.IO (IO)
import GHC.Generics (Generic)
import Text.Show (Show)
import Prelude (undefined)

---------------------------------------------------------------------------------------------------
data Video = Video {
        video_path ::String,
        modified ::Int,
        size ::Int,
        duration ::Int,
        width ::Int,
        height ::Int
    } deriving (Show, Generic)
instance FromJSON Video
instance ToJSON Video

type VideoModelList = [Video]

filename = "gallery-videos.json"

---------------------------------------------------------------------------------------------------
updateCache :: IO ()
updateCache = undefined

---------------------------------------------------------------------------------------------------
loadList :: IO VideoModelList
loadList = loadModelList filename
