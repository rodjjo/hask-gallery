{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Views.HealthChecker (
         healthCheck
        ,HealthCheck
        ,healthCheckReadiness
        ,HealthCheckReadiness
    ) where

import qualified Views.Base as VB

import Data.Aeson (FromJSON, ToJSON)
import Data.Text  (Text)
import Data.Eq (Eq)
import GHC.Generics (Generic)
import Servant.API (Get)
import Servant (JSON)
import Text.Read (Read)
import Text.Show (Show)
import Prelude (fromInteger, return, toInteger, ($), (>=), (/=))
import System.IO (IO)
import Servant.Server.Internal.Handler (Handler(..))

---------------------------------------------------------------------------------------------------
data ServerStatus = ServerStatus
    { status :: Text
    , information :: Text
    } deriving (Eq, Show, Read, Generic)
instance FromJSON ServerStatus
instance ToJSON ServerStatus

---------------------------------------------------------------------------------------------------
type HealthCheck = Get '[JSON] ServerStatus

healthCheck :: VB.GalleryMonad ServerStatus
healthCheck = return $ ServerStatus "alive" "Just livness check"

---------------------------------------------------------------------------------------------------
type HealthCheckReadiness = Get '[JSON] ServerStatus  -- change this

healthCheckReadiness :: VB.GalleryMonad ServerStatus
healthCheckReadiness = return $ ServerStatus "alive" "Not implemented yet"
