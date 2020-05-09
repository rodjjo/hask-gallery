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
import qualified Views.VideoGallery as VG
import qualified Views.File as VF
import Data.Int (Int)
import Data.String (String)
import Servant (ServerT(..), Proxy(..), JSON)
import Servant.API  (Capture(..), CaptureAll(..), Get(..), (:<|>)(..), (:>)(..))
import Prelude (($), return)

--------------------------------------------------------------------------------------------------
type GalleryApi =
    "health-check" :> "liveness" :> HC.HealthCheck
    :<|> "health-check" :> "readiness" :> HC.HealthCheckReadiness
    :<|> "videos" :> Capture "seed" Int :> Capture "page" Int :> VG.GetVideoList
    :<|> "files" :> CaptureAll "path" String :> VF.GetFile

--------------------------------------------------------------------------------------------------
gallery :: Proxy GalleryApi
gallery = Proxy

--------------------------------------------------------------------------------------------------
endpoints :: ServerT GalleryApi VB.GalleryMonad
endpoints =
    a :<|> b :<|> c :<|> d
    where
        a = HC.healthCheck
        b = HC.healthCheckReadiness
        c = VG.getVideoList
        d = VF.getFile
