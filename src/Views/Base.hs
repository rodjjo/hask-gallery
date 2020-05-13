
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module  Views.Base (
         loadInitialState
        ,responseWithMime
        ,lazyResponseWithMime
        ,getVideoGallery
        ,getMusicGallery
        ,getPictureGallery
        ,AllGalleries(..)
        ,State(..)
        ,WithCT(..)
        ,GalleryMonad(..)
    ) where


import qualified Models.Settings as ST
import qualified Models.Video as MV
import qualified Models.Picture as MP
import qualified Models.Music as MM
import qualified Utils as UT

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as B
import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Monad.Trans.Reader  (ReaderT)
import Control.Monad.STM (atomically)
import Data.Maybe
import Data.Text as T
import Network.Mime (defaultMimeLookup)
import Text.Read (Read)
import Prelude (return, ($))
import Servant (OctetStream)
import Servant.API.ContentTypes (AllCTRender(..))
import Servant.Server.Internal.Handler (Handler(..))
import System.IO (IO)
import System.FilePath (FilePath, takeExtension)

type GalleryMonad = ReaderT State Handler

data AllGalleries =  AllGalleries { videos ::MV.VideoGallery
                                  , musics ::MM.MusicGallery
                                  , pics   ::MP.PictureGallery}

data State = State { galleries :: TVar AllGalleries }

--------------------------------------------------------------------------------------------------
loadInitialState :: IO (TVar AllGalleries)
loadInitialState = do
    videosList <- MV.loadList
    musicsList <- MM.loadList
    picsList <- MP.loadList
    galState <- atomically $ newTVar (AllGalleries videosList musicsList picsList)
    return galState

--------------------------------------------------------------------------------------------------
getVideoGallery :: AllGalleries -> MV.VideoGallery
getVideoGallery (AllGalleries v _ _) = v

--------------------------------------------------------------------------------------------------
getMusicGallery :: AllGalleries -> MM.MusicGallery
getMusicGallery (AllGalleries _ m _) = m

--------------------------------------------------------------------------------------------------
getPictureGallery :: AllGalleries -> MP.PictureGallery
getPictureGallery (AllGalleries _ _ p) = p

--------------------------------------------------------------------------------------------------
data WithCT = WithCT { header :: LBS.ByteString, content :: LBS.ByteString }
instance AllCTRender '[OctetStream] WithCT where
  handleAcceptH _ _ (WithCT h c) = Just (h, c)

--------------------------------------------------------------------------------------------------
takeExtensionInLower :: FilePath -> T.Text
takeExtensionInLower filePath =
    T.pack $ UT.lowerPath $ takeExtension filePath

--------------------------------------------------------------------------------------------------
mimeFromPath :: FilePath -> LBS.ByteString
mimeFromPath filePath = LBS.fromChunks [(defaultMimeLookup (takeExtensionInLower filePath))]

--------------------------------------------------------------------------------------------------
responseWithMime :: FilePath -> B.ByteString -> WithCT
responseWithMime filePath rawData =
    WithCT { header=mimeFromPath filePath , content=LBS.fromChunks [rawData] }

--------------------------------------------------------------------------------------------------
lazyResponseWithMime :: FilePath -> LBS.ByteString -> WithCT
lazyResponseWithMime filePath rawData =
    WithCT { header=mimeFromPath filePath, content=rawData }
