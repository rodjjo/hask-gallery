module Application (
        runServer
    ) where

import qualified Routes as RT
import qualified Views.Base as VB

import Control.Applicative (Alternative((<|>)))
import Control.Monad.Trans.Reader  (runReaderT)
import Data.List ((++))
import Data.Int (Int)
import Data.Maybe (fromJust, Maybe(Just))
import Prelude (($))
import qualified Servant as SVT
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server (hoistServer)
import Servant.Server.Internal.Handler (Handler(..))
import System.IO (IO, putStrLn)
import Text.Show (show)

---------------------------------------------------------------------------------------------------
nt :: VB.State -> VB.GalleryMonad a -> Handler a
nt s x = runReaderT x s

app :: VB.State -> SVT.Application
app s = SVT.serve RT.gallery $ hoistServer RT.gallery (nt s) RT.endpoints

---------------------------------------------------------------------------------------------------
runServer :: Maybe Int -> IO ()
runServer port = do
    state <- VB.loadInitialState
    let serverPort = fromJust $ port <|> Just 8080 -- TODO(rodrigo): falback port to an environment variable
    putStrLn("Running server at port " ++ (show serverPort) ++ "\nPress CTRL-C to terminate")
    run serverPort $ app $ VB.State state
    --let runApp = run serverPort $ app $ VB.State state
    -- fork the app if we want to
    --return ()
