{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.MusicGallery (
         getMusicList
        ,GetMusicList
    ) where

import qualified Views.Base as VB
import qualified Models.Music as MM
import qualified Models.Base as MB
import qualified Utils as UT

import Control.Concurrent.STM.TVar (TVar, readTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Text.Show (Show)
import Servant.API (Get)
import Servant (JSON)
import Prelude (return, ($), (/=))

---------------------------------------------------------------------------------------------------

data MusicPayload = MusicPayload
    { items :: MM.MusicList
    , itemCount :: Int
    , totalDuration :: Int
    , pagination :: VB.SimplePagination
    } deriving (Eq, Show, Generic)
instance FromJSON MusicPayload
instance ToJSON MusicPayload

---------------------------------------------------------------------------------------------------
type GetMusicList = Get '[JSON] MusicPayload

getMusicList :: Int -> Word32 -> VB.GalleryMonad MusicPayload
getMusicList  seed unsingedPage = do
    let page = VB.pageToInt unsingedPage

    VB.State { VB.galleries = p } <- ask
    allgalleries <- liftIO $ atomically $ readTVar p
    let gallery = VB.getMusicGallery allgalleries

    randomSeed <- if seed /= 0 then return seed else liftIO $ UT.getRandomSeed
    ( shuffledList, pagination ) <- liftIO $ VB.paginate (MM.getGalleryMusics gallery) randomSeed page 100
    return MusicPayload { items = shuffledList
                         , itemCount = MM.getGalleryCount gallery
                         , totalDuration = MM.getGalleryDuration gallery
                         , pagination = pagination
                         }
