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

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [configurationTests]

configurationTests = testGroup "Configuration"
  [ testCase "Configuration getter functions" $ do
        let settings = SM.Settings "The title" "/path/to/gallery"
        "The title" @?= SM.getTitle settings
        "/path/to/gallery" @?= SM.getVideoGalleryPath settings
  , testCase "Configuration load and save" $ do
        let settings = SM.Settings "title" "/path"
        SM.save settings
        settings2 <- SM.load
        "title" @?= SM.getTitle settings2
        "/path" @?= SM.getVideoGalleryPath settings2
  ]
