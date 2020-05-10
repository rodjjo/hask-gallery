
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
        ,State(..)
        ,WithCT(..)
        ,GalleryMonad(..)
    ) where


import qualified Models.Settings as ST
import qualified Models.Video as MV
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

data State = State { videos :: TVar MV.VideoGallery }

--------------------------------------------------------------------------------------------------
loadInitialState :: IO (TVar MV.VideoGallery)
loadInitialState = do
    videosList <- MV.loadList
    videosListState <- atomically $ newTVar videosList
    return videosListState

--------------------------------------------------------------------------------------------------
data WithCT = WithCT { header :: LBS.ByteString, content :: LBS.ByteString }
instance AllCTRender '[OctetStream] WithCT where
  handleAcceptH _ _ (WithCT h c) = Just (h, c)

--------------------------------------------------------------------------------------------------
takeExtensionInLower :: FilePath -> T.Text
takeExtensionInLower filePath =
    T.pack $ UT.lowerPath $ takeExtension filePath


--------------------------------------------------------------------------------------------------
getHeaderMime :: FilePath -> LBS.ByteString
getHeaderMime filePath = LBS.fromChunks [(defaultMimeLookup (takeExtensionInLower filePath))]

--------------------------------------------------------------------------------------------------
responseWithMime :: FilePath -> B.ByteString -> WithCT
responseWithMime filePath rawData =
    WithCT { header=getHeaderMime filePath , content=LBS.fromChunks [rawData] }

--------------------------------------------------------------------------------------------------
lazyResponseWithMime :: FilePath -> LBS.ByteString -> WithCT
lazyResponseWithMime filePath rawData =
    WithCT { header=getHeaderMime filePath, content=rawData }
