{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.PictureGallery (
         getPictureList
        ,GetPictureList
    ) where

import qualified Views.Base as VB
import qualified Models.Picture as MP
import qualified Models.Base as MB
import qualified Utils as UT

import Control.Concurrent.STM.TVar (TVar, readTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Maybe
import Data.Word (Word32)
import Data.String (String)
import GHC.Generics (Generic)
import Text.Show (Show)
import Servant.API (Get)
import Servant (JSON)
import Prelude (return, ($), (/=))

---------------------------------------------------------------------------------------------------

data PicturePayload = PicturePayload
    { items :: MP.PictureList
    , itemCount :: Int
    , pagination :: VB.SimplePagination
    } deriving (Eq, Show, Generic)
instance FromJSON PicturePayload
instance ToJSON PicturePayload

---------------------------------------------------------------------------------------------------
type GetPictureList = Get '[JSON] PicturePayload

getPictureList :: Int -> Word32 -> Maybe String -> VB.GalleryMonad PicturePayload
getPictureList  seed unsingedPage filter = do
    let page = VB.pageToInt unsingedPage

    VB.State { VB.galleries = p } <- ask
    allgalleries <- liftIO $ atomically $ readTVar p
    let gallery = VB.getPictureGallery allgalleries

    randomSeed <- if seed /= 0 then return seed else liftIO $ UT.getRandomSeed
    ( shuffledList, pagination ) <- liftIO $ VB.paginate (MP.getGalleryPictures gallery) randomSeed page 100
    return PicturePayload { items = shuffledList
                         , itemCount = MP.getGalleryCount gallery
                         , pagination = pagination
                         }
