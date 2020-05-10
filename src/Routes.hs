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
import qualified Views.Static as VS

import Data.Int (Int)
import Data.Word (Word32)
import Data.String (String)
import Prelude (($), return)
import Servant (ServerT(..), Proxy(..), JSON)
import Servant.API  (Capture(..), CaptureAll(..), Get(..), (:<|>)(..), (:>)(..))

--------------------------------------------------------------------------------------------------
type GalleryApi =
    "health-check" :> "liveness" :> HC.HealthCheck
    :<|> "health-check" :> "readiness" :> HC.HealthCheckReadiness
    :<|> "videos" :> Capture "seed" Int :> Capture "page" Int :> VG.GetVideoList
    :<|> "files" :> Capture "gallery" String :>  CaptureAll "path" String :> VF.GetFile
    :<|> CaptureAll "path" String :> VS.GetAsset

--------------------------------------------------------------------------------------------------
gallery :: Proxy GalleryApi
gallery = Proxy

--------------------------------------------------------------------------------------------------
endpoints :: ServerT GalleryApi VB.GalleryMonad
endpoints =
    a :<|> b :<|> c :<|> d :<|> e
    where
        a = HC.healthCheck
        b = HC.healthCheckReadiness
        c = VG.getVideoList
        d = VF.getFile
        e = VS.getAsset -- # keep at last because it capture all
