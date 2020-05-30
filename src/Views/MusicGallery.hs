{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.MusicGallery (
         getMusicList
        ,getMusicCover
        ,GetMusicList
        ,GetMusicCover
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
import Data.List ((++))
import Data.Maybe
import Data.String (String)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Text.Show (Show)
import Servant.API (Get)
import Servant (JSON)
import Prelude (return, ($), (/=))
import System.Directory (doesFileExist)
import System.FilePath.Posix (takeDirectory, (</>))
import System.IO (putStrLn)
---------------------------------------------------------------------------------------------------

data MusicPayload = MusicPayload
    { items :: MM.MusicList
    , itemCount :: Int
    , totalDuration :: Int
    , pagination :: VB.SimplePagination
    } deriving (Eq, Show, Generic)
instance FromJSON MusicPayload
instance ToJSON MusicPayload

data MusicCoverPayload = MusicCoverPayload
    { coverPath :: String
    } deriving (Eq, Show, Generic)
instance FromJSON MusicCoverPayload
instance ToJSON MusicCoverPayload

---------------------------------------------------------------------------------------------------
type GetMusicList = Get '[JSON] MusicPayload

getMusicList :: Int -> Word32 -> Maybe String -> VB.GalleryMonad MusicPayload
getMusicList  seed unsingedPage filter = do
    let page = VB.pageToInt unsingedPage

    VB.State { VB.galleries = p } <- ask
    allgalleries <- liftIO $ atomically $ readTVar p
    let gallery = VB.getMusicGallery allgalleries

    randomSeed <- if seed /= 0 then return seed else liftIO $ UT.getRandomSeed
    ( shuffledList, pagination ) <- liftIO $ VB.paginate (MM.getGalleryMusics gallery filter) randomSeed page 100
    return MusicPayload { items = shuffledList
                         , itemCount = MM.getGalleryCount gallery
                         , totalDuration = MM.getGalleryDuration gallery
                         , pagination = pagination
                         }

---------------------------------------------------------------------------------------------------
type GetMusicCover = Get '[JSON] MusicCoverPayload

getMusicCover :: [String] -> VB.GalleryMonad MusicCoverPayload
getMusicCover paths = do
    let musicPath = UT.relativePathFromList paths
    VB.State { VB.galleries = p } <- ask
    allgalleries <- liftIO $ atomically $ readTVar p
    let gallery = VB.getMusicGallery allgalleries
    if UT.isPathUnSafe musicPath
        then return $ MusicCoverPayload ""
        else do
            let dir = MM.getGalleryPath gallery
            let musicDir = UT.dropFirstSlash $ takeDirectory ("/" ++ musicPath)
            let coverPath = dir </> musicDir </> "cover.jpg"
            existence <- liftIO $ doesFileExist coverPath
            if existence
                then return $ MusicCoverPayload ("/files/musics" </> musicDir </> "cover.jpg")
                else return $ MusicCoverPayload ""
