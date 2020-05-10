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
import Data.Text as T
import Prelude (return, ($), (==))
import Network.Mime (defaultMimeLookup)
import Servant.API (Get)
import Servant.API.ContentTypes (AllCTRender(..))
import Servant (OctetStream)
import System.IO (IO)
import Servant.Server.Internal.Handler (Handler(..))
import System.FilePath (FilePath, takeExtension)

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
data WithCT = WithCT { header :: LBS.ByteString, content :: LBS.ByteString }
instance AllCTRender '[OctetStream] WithCT where
  handleAcceptH _ _ (WithCT h c) = Just (h, c)

---------------------------------------------------------------------------------------------------
takeExtensionNoFirstDot :: String -> T.Text
takeExtensionNoFirstDot filePath =
    T.pack $ UT.lowerPath $ takeExtension filePath

type GetAsset = Get '[OctetStream] WithCT

getAsset :: [String] -> VB.GalleryMonad WithCT
getAsset path = do
    let filePath = UT.dropFirstSlash $ UT.filePathFromList path
    let contents = findEmbeddedAsset (if filePath `elem` ["index.html", ""] then "index.html" else filePath)
    if contents == Nothing
        then return $ WithCT {header = "text/plain", content = "Not Found!"}
        else do
                let (_ , rawData) = fromJust contents
                return $ WithCT {
                    header = LBS.fromChunks [(defaultMimeLookup (takeExtensionNoFirstDot filePath))]
                    , content = LBS.fromChunks [rawData]
                    }
