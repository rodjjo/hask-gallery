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
import qualified Views.Base as VB
import Servant (ServerT(..), Proxy(..), JSON)
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
endpoints :: ServerT GalleryApi VB.GalleryMonad
endpoints = a :<|> b
    where
        a = HC.healthCheck
        b = HC.healthCheckReadiness
