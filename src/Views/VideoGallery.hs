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
    , durationTotal :: Int
    , randomSeed :: Int
    } deriving (Eq, Show, Generic)

instance FromJSON VideosPagination
instance ToJSON VideosPagination

data VideosPayload = VideosPayload
    { videos :: MV.VideoList
    , pagination :: VideosPagination
    } deriving (Eq, Show, Generic)
instance FromJSON VideosPayload
instance ToJSON VideosPayload

---------------------------------------------------------------------------------------------------
type GetVideoList = Get '[JSON] VideosPayload

getVideoList :: Int -> Int -> VB.GalleryMonad VideosPayload
getVideoList seed page = do
    let pa = VideosPagination 0 0 0 0
    return $ VideosPayload [] pa
