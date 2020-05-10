{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.File (
         getFile
        ,GetFile
    ) where

import qualified Models.Video as MV
import qualified Views.Base as VB
import qualified Utils as UT

import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Lazy as LBS
import Data.String (String)
import Servant.API (GetPartialContent)
import Prelude (return, ($))
import System.IO (IO)
import Servant (OctetStream)
import Servant (serveDirectory)
import Servant.Server.Internal.Handler (Handler(..))
import System.FilePath.Posix ((</>))

---------------------------------------------------------------------------------------------------
type  GetFile = GetPartialContent '[OctetStream] VB.WithCT

getFile :: String -> [String] -> VB.GalleryMonad VB.WithCT
getFile galleryName path = do  -- galleryName wiil be used to switch between video, music and photo galleries
    VB.State { VB.videos = p } <- ask
    gallery <- liftIO $ atomically $ readTVar p
    let filePath = (MV.getGalleryPath gallery) </> (UT.relativePathFromList path)
    return $ VB.lazyResponseWithMime filePath LBS.empty
