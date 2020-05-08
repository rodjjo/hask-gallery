{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Routes (
         gallery
        ,endpoints
    ) where

import qualified Views.HealthChecker as HC
import Servant (Server(..), Proxy(..), JSON)
import Servant.API  (Get(..), (:<|>)(..), (:>)(..))
import Prelude (($), return)

--------------------------------------------------------------------------------------------------
type GalleryApi =
    "health-check" :> "liveness" :> HC.HealthCheck
    :<|> "health-check" :> "readiness" :> HC.HealthCheckReadiness

--------------------------------------------------------------------------------------------------
gallery :: Proxy GalleryApi
gallery = Proxy

--------------------------------------------------------------------------------------------------
endpoints :: Server GalleryApi
endpoints = a :<|> b
    where
        a = return $ HC.healthCheck
        b = return $ HC.healthCheckReadiness
