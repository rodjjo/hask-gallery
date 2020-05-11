module Application (
        runServer
    ) where

import qualified Routes as RT
import qualified Views.Base as VB

import Control.Applicative (Alternative((<|>)))
import Control.Monad.Trans.Reader  (runReaderT)
import Data.List ((++))
import Data.Int (Int)
import Data.Maybe (fromJust, Maybe(..))
import Prelude (return, ($), (==))
import qualified Servant as SVT
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Server (hoistServer)
import Servant.Server.Internal.Handler (Handler(..))
import System.IO (IO, putStrLn)
import Text.Show (show)
import Text.Read (readMaybe)
import System.Environment (lookupEnv)

---------------------------------------------------------------------------------------------------
nt :: VB.State -> VB.GalleryMonad a -> Handler a
nt s x = runReaderT x s

app :: VB.State -> SVT.Application
app s = SVT.serve RT.gallery $ hoistServer RT.gallery (nt s) RT.endpoints

---------------------------------------------------------------------------------------------------
portFromEnv :: Int -> IO Int
portFromEnv def = do
    mport <- lookupEnv "PORT"
    if mport == Nothing
        then return def
        else return $ (fromJust ((readMaybe $ fromJust mport) <|> Just def))

---------------------------------------------------------------------------------------------------
runServer :: Maybe Int -> IO ()
runServer port = do
    state <- VB.loadInitialState
    envPort <- portFromEnv 8080
    let serverPort = fromJust $ port <|> Just envPort
    putStrLn("Running server at port " ++ (show serverPort) ++ "\nPress CTRL-C to terminate")
    run serverPort $ app $ VB.State state
