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

import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text  (Text)
import Data.Int (Int)
import Data.Eq (Eq)
import GHC.Generics (Generic)
import Servant.API (Get)
import Servant (JSON)
import Text.Read (Read)
import Text.Show (Show)
import Prelude (return, ($))
import System.IO (IO)
import Servant.Server.Internal.Handler (Handler(..))

---------------------------------------------------------------------------------------------------

data VideosPagination = VideosPagination
    { page :: Int
    , pageCount :: Int
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
type GetVideoList = Get '[JSON] VideosPayload

getVideoList :: Int -> Int -> VB.GalleryMonad VideosPayload
getVideoList seed page = do
    VB.State { VB.videos = p } <- ask
    gallery <- liftIO $ atomically $ readTVar p
    let payload = VideosPayload { items = MV.getGalleryVideos gallery
                                , itemCount = MV.getGalleryCount gallery
                                , totalDuration = MV.getGalleryDuration gallery
                                , pagination = ( VideosPagination 0 0 0 )
                                }
    return payload
