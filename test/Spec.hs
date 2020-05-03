{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE NoImplicitPrelude #-}

import qualified SettingsModel as SM

import Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import Prelude (($))
import System.IO (IO, putStrLn)

---------------------------------------------------------------------------------------------------
main = defaultMain tests

---------------------------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Tests" [configurationTests]

---------------------------------------------------------------------------------------------------
configurationTests = testGroup "Configuration"
  [ testCase "Configuration getter functions" $ do
        let settings = SM.new
        "Hask Gallery" @?= SM.getTitle settings
        "" @?= SM.getVideoGalleryPath settings
  , testCase "Configuration load and save" $ do
        let settings = SM.setVideoGalleryPath (SM.setTitle SM.new "ChangedTitle") "ChangedPath"
        SM.save settings
        settings2 <- SM.load
        "ChangedTitle" @?= SM.getTitle settings2
        "ChangedPath" @?= SM.getVideoGalleryPath settings2
  ]
