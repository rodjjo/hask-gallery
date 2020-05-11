{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Views.Static (
         getAsset
        ,GetAsset
    ) where

import qualified Views.Base as VB
import qualified Utils as UT

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as B
import Data.FileEmbed (embedDir)
import Data.String (String)
import Data.List (elem, (++))
import Data.Maybe
import Prelude (return, ($), (==))
import Servant.API (Get)
import Servant (OctetStream)
import System.IO (IO)
import Servant.Server.Internal.Handler (Handler(..))
import System.FilePath (FilePath)

--------------------------------------------------------------------------------------------------
assets :: [(FilePath, B.ByteString)]
assets = $(embedDir "assets")

--------------------------------------------------------------------------------------------------
replaceSeparator :: String -> String
replaceSeparator [] = []
replaceSeparator (x:xs) = if x == '\\' then ('/':xs) else  [x] ++ replaceSeparator xs

--------------------------------------------------------------------------------------------------
extractPath :: (FilePath, B.ByteString) -> FilePath
extractPath (p1, _) = replaceSeparator p1

--------------------------------------------------------------------------------------------------
findAsset :: [(FilePath, B.ByteString)] -> String -> Maybe (FilePath, B.ByteString)
findAsset [] filePath = Nothing
findAsset (x:xs) filePath =
    if (extractPath x) == filePath then Just x else findAsset xs filePath

findEmbeddedAsset = findAsset assets

---------------------------------------------------------------------------------------------------
type GetAsset = Get '[OctetStream] VB.WithCT

getAsset :: [String] -> VB.GalleryMonad VB.WithCT
getAsset path = do
    let filePath = UT.relativePathFromList path
    let contents = findEmbeddedAsset (if filePath `elem` ["index.htm", ""] then "index.html" else filePath)
    if contents == Nothing
        then return $ VB.WithCT {VB.header = "text/plain", VB.content = "Not Found!"}
        else do
                let (_ , rawData) = fromJust contents
                return $ VB.responseWithMime (if filePath == "" then ".html" else filePath) rawData
