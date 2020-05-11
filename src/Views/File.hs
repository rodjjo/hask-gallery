{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Views.File (
         getFile
        ,GetFile
    ) where

import qualified Models.Video as MV
import qualified Views.Base as VB
import qualified Utils as UT
import qualified Stream.File as SF

import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Lazy as LBS
import Data.String (String)
import Data.List (elem, (++))
import Data.Int (Int)
import Data.Maybe
import Servant.API (Get, GetPartialContent, (:>))
import qualified Servant.API.ResponseHeaders as RH
import qualified Servant.API.Header as RQ
import Prelude (return, read, ($), (<$>), (==), (/=), (>=), (<), (+), (-))
import System.IO (IO, putStrLn)
import Servant (OctetStream)
import Servant (serveDirectory)
import Servant.Server.Internal.Handler (Handler(..))
import System.FilePath.Posix ((</>))
import System.FilePath (FilePath)
import Text.Show (show)

---------------------------------------------------------------------------------------------------
type  FileRangeResponse = (RH.Headers ContentHeaders VB.WithCT)
type  GetFile = GetPartialContent '[OctetStream] FileRangeResponse

type ContentHeaders =
    '[ RQ.Header "Accept-Ranges" String
     , RQ.Header "Content-Length" String
     , RQ.Header "Content-Range" String
     ]

---------------------------------------------------------------------------------------------------
beforeDash :: String -> String
beforeDash [] = []
beforeDash (s:m:xs) = [s] ++ if m == '-' then [] else beforeDash (m:xs)
beforeDash _ = []

---------------------------------------------------------------------------------------------------
afterDash :: String -> String
afterDash [] = []
afterDash (s:xs) = if '-' `elem` xs then afterDash xs else xs

---------------------------------------------------------------------------------------------------
defaultIfEmpty :: String -> String -> String
defaultIfEmpty def value  = if value == "" then def else value

---------------------------------------------------------------------------------------------------
parseRange :: String -> (Int, Int)
parseRange (' ':xs) = parseRange xs
parseRange ('=':xs) = parseRange xs
parseRange ('b':'y':'t':'e':'s':xs) = parseRange xs
parseRange ('-':xs) = ((read (['-'] ++ xs) ::Int), 0)
parseRange mrange =
    if '-' `elem` mrange
        then
            (
                (read $ defaultIfEmpty "0" $ beforeDash mrange) ::Int,
                ((read $ defaultIfEmpty "-1" $ afterDash mrange) ::Int) + 1)
        else (0, 0)

---------------------------------------------------------------------------------------------------
adaptRange :: Int -> ( Int, Int ) -> Maybe ( Int, Int )
adaptRange fileSize (start, stop) =
    if start < 0
        then (if (fileSize + start) >= 0 then Just (fileSize + start, fileSize) else Just (0, fileSize))
        else (if ((start + stop) == 0) then Nothing else (
                    if stop >= start
                        then Just (start, stop)
                        else Just (start, fileSize)
                ))

---------------------------------------------------------------------------------------------------
seekPosLimitFromRange :: (Int, Int) -> (Int, Int)
seekPosLimitFromRange (rstart, rstop) = (rstart, rstop - rstart)

---------------------------------------------------------------------------------------------------
serveFileAtRange :: Maybe String -> FilePath -> VB.GalleryMonad FileRangeResponse
serveFileAtRange requestRange filePath = do
    fileSize <- liftIO $ SF.getFileSize filePath
    let mRange = (
            if requestRange /= Nothing
                then adaptRange fileSize $ parseRange $ fromJust requestRange
                else Nothing)

    let (seekPos, limit) = (
            if mRange == Nothing
                then ( 0, fileSize )
                else (seekPosLimitFromRange $ fromJust mRange))

    fileContents <- liftIO $ SF.streamFile filePath seekPos limit
    let rangeHeader = ("bytes " ++ (show seekPos) ++ "-" ++ (show (seekPos + limit - 1)) ++ "/" ++ (show fileSize))
    return
        $ RH.addHeader "bytes"
        $ RH.addHeader (show (limit))
        $ RH.addHeader rangeHeader
        $ VB.lazyResponseWithMime filePath fileContents

---------------------------------------------------------------------------------------------------
emptyResponse :: VB.GalleryMonad FileRangeResponse
emptyResponse = do
        return
            $ RH.addHeader "bytes"
            $ RH.addHeader "0"
            $ RH.addHeader "0-0-0"
            $ VB.lazyResponseWithMime ".txt" LBS.empty

---------------------------------------------------------------------------------------------------
getFile :: Maybe String -> String -> [String] -> VB.GalleryMonad FileRangeResponse
getFile mrange galleryName path = do  -- galleryName wiil be used to switch between video, music and photo galleries
    VB.State { VB.videos = p } <- ask
    case galleryName of
        ("videos") -> do
            gallery <- liftIO $ atomically $ readTVar p
            let filePath = (MV.getGalleryPath gallery) </> (UT.relativePathFromList path)
            if UT.isPathUnSafe path
                then do
                    liftIO $ putStrLn ("Unsafe path: " ++ filePath)
                    emptyResponse
                else if MV.getGalleryPath gallery == ""
                        then do
                            liftIO $ putStrLn "The video gallery path was not configured"
                            emptyResponse
                        else do
                            serveFileAtRange mrange filePath
        (_) -> do  -- TODO: create other galleries (music and pictures)
            emptyResponse
