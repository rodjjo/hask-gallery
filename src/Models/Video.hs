{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Models.Video (
         updateCache
        ,loadList
        ,Video(..)
        ,VideoList
        ,VideoGallery
    ) where

import Models.Base (
         loadModel
        ,saveModel
    )

import qualified Probe.FFProbe as PB

import Scan.ScanMedia (
        searchForVideos
    )

import Utils (
         getModifiedTime
        ,quicksort
        ,quicksortM
        ,dropFirstSlash
    )

import Control.Monad (mapM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Ord (Ord)
import Data.List (head, drop, foldl, (++))
import Data.String (String)
import Data.Maybe (fromJust, Maybe(Nothing))
import GHC.Generics (Generic)
import Prelude (Ordering, compare, length, otherwise, return, undefined, (==), (<), (>), ($), (/=), (+))
import Text.Show (Show, show)
import System.FilePath (dropFileName, FilePath, takeExtension)
import System.FilePath.Posix ((</>))
import System.IO (IO, putStrLn)

---------------------------------------------------------------------------------------------------
data Video = Video { videoPath ::String
                   , modified ::Int
                   , duration ::Int
                   , width ::Int
                   , height ::Int
                   } deriving (Show, Generic, Eq)
instance FromJSON Video
instance ToJSON Video

type VideoList = [Video]

---------------------------------------------------------------------------------------------------
data VideoGallery = VideoGallery { galleryItems ::VideoList
                                 , baseFilePath ::String
                                 , totalCount ::Int
                                 , totalDuration ::Int
                                 } deriving (Show, Generic, Eq)
instance FromJSON VideoGallery
instance ToJSON VideoGallery

filename = "gallery-videos.hgl"

emptyGallery = VideoGallery ([] ::VideoList) "" 0 0

---------------------------------------------------------------------------------------------------
getVideoPath :: Video -> String
getVideoPath (Video path _ _ _ _ ) = path

---------------------------------------------------------------------------------------------------
videoModifiedAt :: Video -> Int
videoModifiedAt (Video _ modified _ _ _ ) = modified

---------------------------------------------------------------------------------------------------
videoDuration :: Video -> Int
videoDuration (Video _ _ d _ _ ) = d

---------------------------------------------------------------------------------------------------
instance Ord Video where
    v1 `compare` v2 = (getVideoPath v1) `compare` (getVideoPath v2)

---------------------------------------------------------------------------------------------------
loadList :: IO VideoGallery
loadList = loadModel filename emptyGallery

---------------------------------------------------------------------------------------------------
newVideo :: String -> Video
newVideo path = Video path 0 0 0 0

---------------------------------------------------------------------------------------------------
metaDataToVideo :: FilePath -> IO Int -> PB.MediaInfo ->  IO Video
metaDataToVideo path modifiedIO metaData = do
    modifiedTime <- modifiedIO
    let (vwidth, vheight) = PB.getDimensions metaData
    putStrLn ("Duration: " ++ (show (PB.getDuration metaData)) ++ "secs Width: " ++ (show vwidth) ++ " height: " ++ (show vheight))
    return Video {
            videoPath=path
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
    if modifiedPathTime /= (videoModifiedAt video)
        then videoInfo $ getVideoPath video
        else return video

---------------------------------------------------------------------------------------------------
addVideo :: IO Video -> IO [Video] -> IO [Video]
addVideo iovideo iovideos = do
    video <- iovideo
    videos <- iovideos
    return ([video] ++ videos)

---------------------------------------------------------------------------------------------------
updatedVideoList :: [FilePath] -> VideoList -> IO VideoList
updatedVideoList [] videolist = return []
updatedVideoList sortedpaths [] = mapM (videoInfo) sortedpaths
updatedVideoList (hp:sortedpaths) (hv:videos)
    | hp < getVideoPath hv = updatedVideoList (hp:sortedpaths) videos
    | hp == getVideoPath hv = addVideo (refreshVideoInfo hv) $ updatedVideoList sortedpaths videos
    | otherwise = updatedVideoList (hp:sortedpaths) ((newVideo hp) : hv : videos)

---------------------------------------------------------------------------------------------------
decomposeVideoGallery :: VideoGallery -> (VideoList, String, Int, Int)
decomposeVideoGallery (VideoGallery p1 p2 p3 p4) = (p1, p2, p3, p4)

videoAtBasePath :: String -> Video -> Video
videoAtBasePath path (Video p1 p2 p3 p4 p5) =
    Video (path </> dropFirstSlash p1) p2 p3 p4 p5

videoCutBasePath :: String -> Video -> Video
videoCutBasePath path (Video p1 p2 p3 p4 p5) =
    Video (drop (length path) p1) p2 p3 p4 p5

videoListInfo :: VideoList -> (Int, Int)
videoListInfo theList =
    foldl (\(duration, count) video -> (( (duration + videoDuration video) ::Int), (count + 1) ::Int)) (0, 0) theList

updateCacheInternal :: String -> IO ()
updateCacheInternal gallerpath = do
    currentlist <- loadList
    let (theItems, basePath, _, _) = decomposeVideoGallery currentlist
    let validatedItems =
            if basePath == gallerpath
                then [ videoAtBasePath gallerpath v | v <- theItems ]
                else []
    let first = head validatedItems
    sortedPaths <- quicksortM $ searchForVideos gallerpath
    models <- updatedVideoList sortedPaths validatedItems
    let newItems = [ videoCutBasePath gallerpath  m | m <- models, (videoModifiedAt m) /= 0]
    let (duration, count) = videoListInfo newItems
    let newGallery = VideoGallery { galleryItems = newItems
                                  , baseFilePath = gallerpath
                                  , totalCount = count
                                  , totalDuration = duration
                                  }
    saveModel filename newGallery
    putStrLn "The video gallery was updated."

---------------------------------------------------------------------------------------------------
updateCache :: String -> IO ()
updateCache gallerpath = do
    hasProbe <- PB.probeInstalled
    if hasProbe
        then updateCacheInternal gallerpath
        else putStrLn "ffprobe was not found in your machine.\nPlease install and configure PATH environment variable\nYou also can download and put the ffprobe aside hask-gallery executable."
