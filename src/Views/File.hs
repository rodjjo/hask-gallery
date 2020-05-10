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

import Data.String (String)
import Servant.API (Get)
import Prelude (return, ($))
import System.IO (IO)
import Servant (PlainText)
import Servant (serveDirectory)
import Servant.Server.Internal.Handler (Handler(..))

---------------------------------------------------------------------------------------------------
--type VF.RawDirectory = Raw
type  GetFile = Get '[PlainText] String

getFile :: [String] -> VB.GalleryMonad String
getFile filePath = do
    return "Not implemented yet"
