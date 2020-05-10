{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.File (
         getFile
        ,GetFile
    ) where

import qualified Views.Base as VB
import qualified Utils as UT

import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ask)
import Data.String (String)
import Servant.API (GetPartialContent)
import Prelude (return, ($))
import System.IO (IO)
import Servant (OctetStream)
import Servant (serveDirectory)
import Servant.Server.Internal.Handler (Handler(..))

---------------------------------------------------------------------------------------------------
--type VF.RawDirectory = Raw
type  GetFile = GetPartialContent '[OctetStream] VB.WithCT

getFile :: [String] -> VB.GalleryMonad VB.WithCT
getFile filePath = do
    VB.State { VB.videos = p } <- ask
    gallery <- liftIO $ atomically $ readTVar p
    return $ VB.responseWithMime ".html" "<b>Not implemented yet</b>"
