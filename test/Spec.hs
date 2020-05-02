{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE NoImplicitPrelude #-}

import GalleryModels

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
        let settings = ConfigurationModel "The title" "/path/to/gallery"
        "The title" @?= configurationTitle settings
        "/path/to/gallery" @?= configurationGalleryPath settings
  , testCase "Configuration load and save" $ do
        let settings = ConfigurationModel "title" "/path"
        setConfiguration settings
        settings2 <- getConfiguration
        "title" @?= configurationTitle settings2
        "/path" @?= configurationGalleryPath settings2
  ]
