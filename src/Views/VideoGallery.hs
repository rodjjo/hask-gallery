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

import Control.Concurrent.STM.TVar (TVar, readTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text  (Text)
import Data.Int (Int)
import Data.Word (Word32)
import Data.Eq (Eq)
import Data.List (take, drop)
import Data.Maybe
import Data.String (String)
import GHC.Generics (Generic)
import Servant.API (Get)
import Servant (JSON)
import Text.Read (Read)
import Text.Show (Show)
import Prelude (return, ($), (/=))
import System.IO (IO)
import Servant.Server.Internal.Handler (Handler(..))

---------------------------------------------------------------------------------------------------
data VideosPayload = VideosPayload
    { items :: MV.VideoList
    , itemCount :: Int
    , totalDuration :: Int
    , pagination :: VB.SimplePagination
    } deriving (Eq, Show, Generic)
instance FromJSON VideosPayload
instance ToJSON VideosPayload

---------------------------------------------------------------------------------------------------
type GetVideoList = Get '[JSON] VideosPayload

getVideoList :: Int -> Word32 -> Maybe String -> VB.GalleryMonad VideosPayload
getVideoList seed unsingedPage filter = do
    let page = VB.pageToInt unsingedPage

    VB.State { VB.galleries = p } <- ask
    allgalleries <- liftIO $ atomically $ readTVar p
    let gallery = VB.getVideoGallery allgalleries

    randomSeed <- if seed /= 0 then return seed else liftIO $ UT.getRandomSeed
    ( shuffledList, pagination ) <- liftIO $ VB.paginate (MV.getGalleryVideos gallery filter) randomSeed page 100
    return VideosPayload { items = shuffledList
                         , itemCount = MV.getGalleryCount gallery
                         , totalDuration = MV.getGalleryDuration gallery
                         , pagination = pagination
                         }
