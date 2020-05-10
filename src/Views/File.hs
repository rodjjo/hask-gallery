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
import qualified Stream.File as SF

import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Lazy as LBS
import Data.String (String)
import Servant.API (Get, GetPartialContent)
import Prelude (return, ($))
import System.IO (IO)
import Servant (OctetStream)
import Servant (serveDirectory)
import Servant.Server.Internal.Handler (Handler(..))
import System.FilePath.Posix ((</>))

---------------------------------------------------------------------------------------------------
type  GetFile = Get '[OctetStream] VB.WithCT

getFile :: String -> [String] -> VB.GalleryMonad VB.WithCT
getFile galleryName path = do  -- galleryName wiil be used to switch between video, music and photo galleries
    VB.State { VB.videos = p } <- ask
    case galleryName of
        ("videos") -> do
            gallery <- liftIO $ atomically $ readTVar p
            let filePath = (MV.getGalleryPath gallery) </> (UT.relativePathFromList path)
            fileContents <- liftIO $ SF.streamFile filePath 0 -- TODO: replace ZERO with a range to seek
            return $ VB.lazyResponseWithMime filePath fileContents
        (_) -> do  -- TODO: create other galleries (music and pictures)
            return $ VB.lazyResponseWithMime ".txt" LBS.empty
