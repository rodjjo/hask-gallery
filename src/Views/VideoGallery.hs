{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.VideoGallery (
         getVideoList
        ,GetVideoList
    ) where

import qualified Views.Base as VB
import qualified Models.Video as MV
import qualified Models.Base as MB
import qualified Utils as UT

import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text  (Text)
import Data.Int (Int)
import Data.Eq (Eq)
import Data.List (take, drop)
import GHC.Generics (Generic)
import Servant.API (Get)
import Servant (JSON)
import Text.Read (Read)
import Text.Show (Show)
import Prelude (Double, ceiling, fromInteger, length, return, toInteger, ($), (>=), (/=), (*), (-), (/))
import System.IO (IO)
import Servant.Server.Internal.Handler (Handler(..))

---------------------------------------------------------------------------------------------------

data VideosPagination = VideosPagination
    { page :: Int
    , pageCount :: Int
    , maxItemsPerPage :: Int
    , randomSeed :: Int
    } deriving (Eq, Show, Generic)

instance FromJSON VideosPagination
instance ToJSON VideosPagination

data VideosPayload = VideosPayload
    { items :: MV.VideoList
    , itemCount :: Int
    , totalDuration :: Int
    , pagination :: VideosPagination
    } deriving (Eq, Show, Generic)
instance FromJSON VideosPayload
instance ToJSON VideosPayload


---------------------------------------------------------------------------------------------------
toDouble :: Int -> Double
toDouble n1 = fromInteger $ toInteger n1

---------------------------------------------------------------------------------------------------
shuffleVideoList :: MV.VideoList -> Int -> MV.VideoList
shuffleVideoList theList theSeed = MB.shuffleModelList theList theSeed

---------------------------------------------------------------------------------------------------
paginateVideoList ::  MV.VideoList -> Int -> Int -> Int -> IO (MV.VideoList, VideosPagination)
paginateVideoList theList theSeed thePage thePageSize = do
    let shuffledList = shuffleVideoList theList theSeed
    let maxPages = ceiling ((toDouble (length shuffledList)) / (toDouble  thePageSize))
    let finalPage = if thePage >= maxPages then maxPages - 1 else thePage
    let pageItems = take thePageSize $ drop (finalPage * thePageSize) shuffledList
    return (pageItems, VideosPagination finalPage maxPages thePageSize theSeed)

---------------------------------------------------------------------------------------------------
type GetVideoList = Get '[JSON] VideosPayload

getVideoList :: Int -> Int -> VB.GalleryMonad VideosPayload
getVideoList seed page = do
    VB.State { VB.videos = p } <- ask
    gallery <- liftIO $ atomically $ readTVar p
    randomSeed <- if seed /= 0 then return seed else liftIO $ UT.getRandomSeed
    ( shuffledList, pagination ) <- liftIO $ paginateVideoList (MV.getGalleryVideos gallery) randomSeed page 100
    return VideosPayload { items = shuffledList
                         , itemCount = MV.getGalleryCount gallery
                         , totalDuration = MV.getGalleryDuration gallery
                         , pagination = pagination
                         }
