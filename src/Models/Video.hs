{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Models.Video (
         updateCache
        ,loadList
        ,Video(..)
    ) where

import Models.Base (
         loadModel
        ,saveModel
        ,saveModelList
        ,loadModelList
    )

import qualified Probe.FFProbe as PB

import Scan.ScanMedia (
        searchForVideos
    )

import Utils (
         getModifiedTime
        ,quicksort
        ,quicksortM
    )

import Control.Monad (mapM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Ord (Ord)
import Data.List ((++))
import Data.String (String)
import Data.Maybe (fromJust, Maybe(Nothing))
import GHC.Generics (Generic)
import Prelude (Ordering, compare, otherwise, return, undefined, (==), (<), (>), ($), (/=))
import Text.Show (Show, show)
import System.FilePath ((</>), dropFileName, FilePath, takeExtension)
import System.IO (IO, putStrLn)

---------------------------------------------------------------------------------------------------
data Video = Video {
        video_path ::String,
        modified ::Int,
        duration ::String,
        width ::Int,
        height ::Int
    } deriving (Show, Generic, Eq)
instance FromJSON Video
instance ToJSON Video

type VideoList = [Video]

filename = "gallery-videos.json"

---------------------------------------------------------------------------------------------------
getVideoPath :: Video -> String
getVideoPath (Video path _ _ _ _ ) = path

---------------------------------------------------------------------------------------------------
getVideoTime :: Video -> Int
getVideoTime (Video _ modified _ _ _ ) = modified

---------------------------------------------------------------------------------------------------
instance Ord Video where
    v1 `compare` v2 = (getVideoPath v1) `compare` (getVideoPath v2)

---------------------------------------------------------------------------------------------------
loadList :: IO VideoList
loadList = loadModelList filename

---------------------------------------------------------------------------------------------------
newVideo :: String -> Video
newVideo path = Video { video_path=path }


---------------------------------------------------------------------------------------------------
metaDataToVideo :: FilePath -> IO Int -> PB.MediaInfo ->  IO Video
metaDataToVideo path modifiedIO metaData = do
    modifiedTime <- modifiedIO
    let (vwidth, vheight) = PB.getDimensions metaData
    putStrLn ("Duration: " ++ (PB.getDuration metaData) ++ " Width: " ++ (show vwidth) ++ " height: " ++ (show vheight))
    return Video {
            video_path=path
            ,modified=modifiedTime
            ,duration=(PB.getDuration metaData)
            ,width=vwidth
            ,height=vheight
        }

---------------------------------------------------------------------------------------------------
videoInfo :: FilePath -> IO Video
videoInfo path = do
    putStrLn ("Gathering information about video: " ++ path ++ " ...")
    videoData <- PB.probeFile path
    if videoData == Nothing
        then putStrLn "Could not retrive informations :_("
        else return ()
    if videoData == Nothing
        then return $ newVideo path
        else  metaDataToVideo path (getModifiedTime path) (fromJust videoData)

---------------------------------------------------------------------------------------------------
refreshVideoInfo :: Video -> IO Video
refreshVideoInfo video = do
    modifiedPathTime <- getModifiedTime $ getVideoPath video
    if modifiedPathTime /= (getVideoTime video)
        then videoInfo $ getVideoPath video
        else return video

---------------------------------------------------------------------------------------------------
addVideo :: IO Video -> IO [Video] -> IO [Video]
addVideo iovideo iovideos = do
    video <- iovideo
    videos <- iovideos
    return ([video] ++ videos)

---------------------------------------------------------------------------------------------------
updatedModelList :: [FilePath] -> VideoList -> IO VideoList
updatedModelList [] videolist = return []
updatedModelList sortedpaths [] = mapM (videoInfo) sortedpaths
updatedModelList (hp:sortedpaths) (hv:videos)
    | hp < getVideoPath hv = updatedModelList (hp:sortedpaths) videos
    | hp == getVideoPath hv = addVideo (refreshVideoInfo hv) $ updatedModelList sortedpaths videos
    | otherwise = updatedModelList (hp:sortedpaths) ((newVideo hp) : hv : videos)

---------------------------------------------------------------------------------------------------
updateCacheInternal :: String -> IO ()
updateCacheInternal gallerpath = do
    currentlist <- loadList
    paths <- quicksortM $ searchForVideos gallerpath
    models <- updatedModelList paths currentlist
    saveModelList filename [ m | m <- models, (getVideoTime m) /= 0]
    putStrLn "The video gallery was updated."

---------------------------------------------------------------------------------------------------
updateCache :: String -> IO ()
updateCache gallerpath = do
    hasProbe <- PB.probeInstalled
    if hasProbe
        then updateCacheInternal gallerpath
        else putStrLn "ffprobe was not found in your machine.\nPlease install and configure PATH environment variable\nYou also can download and put the ffprobe aside hask-gallery executable."
