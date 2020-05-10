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

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Eq (Eq)
import Data.String (String)
import Data.List (foldl, (++))
import GHC.Generics (Generic)
import Servant.API (Get)
import Servant (JSON)
import Text.Read (Read)
import Text.Show (Show)
import Prelude (return, ($))
import System.IO (IO)
import Servant.Server.Internal.Handler (Handler(..))

---------------------------------------------------------------------------------------------------
data ServerStatus = ServerStatus
    { status :: Text
    , information :: String
    } deriving (Eq, Show, Read, Generic)
instance FromJSON ServerStatus
instance ToJSON ServerStatus

---------------------------------------------------------------------------------------------------
type GetFile = Get '[JSON] ServerStatus

getFile :: [String] -> VB.GalleryMonad ServerStatus
getFile filePath = do
    return $ ServerStatus "info" $ UT.filePathFromList filePath
