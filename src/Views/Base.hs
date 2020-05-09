module  Views.Base (
         GalleryMonad(..)
        ,loadInitialState
        ,State(..)
    ) where


import qualified Models.Settings as ST
import qualified Models.Video as MV

import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Monad.Trans.Reader  (ReaderT)
import Control.Monad.STM (atomically)
import Text.Read (Read)
import Prelude (return, ($))
import System.IO (IO)
import Servant.Server.Internal.Handler (Handler(..))

type GalleryMonad = ReaderT State Handler

data State = State { videos :: TVar MV.VideoList }

loadInitialState :: IO (TVar MV.VideoList)
loadInitialState = do
    videosList <- MV.loadList
    videosListState <- atomically $ newTVar videos
    return videosListState
