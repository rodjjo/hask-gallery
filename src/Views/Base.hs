module  Views.Base (
         GalleryMonad(..)
        ,loadInitialState
        ,State(..)
    ) where


import qualified Models.Settings as ST
import qualified Models.Video as MV

import Control.Concurrent.STM.TVar (TVar, newTVar)
import Control.Monad.Trans.Reader  (ReaderT)
--import Control.Monad.Except (ExceptT(..))
import Control.Monad.STM (atomically)
import Text.Read (Read)
import Prelude (return, ($))
import System.IO (IO)
import Servant.Server.Internal.Handler (Handler(..))

type GalleryMonad = ReaderT State Handler

data State = State { videos :: TVar MV.VideoGallery }

loadInitialState :: IO (TVar MV.VideoGallery)
loadInitialState = do
    videosList <- MV.loadList
    videosListState <- atomically $ newTVar videosList
    return videosListState
