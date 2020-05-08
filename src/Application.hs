module Application (
        runServer
    ) where

import qualified Routes as RT
import Control.Applicative (Alternative((<|>)))

import Data.List ((++))
import Data.Int (Int)
import Data.Maybe (fromJust, Maybe(Just))
import Prelude (($))
import qualified Servant as SVT
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API as API
import System.IO (IO, putStrLn)
import Text.Show (show)

---------------------------------------------------------------------------------------------------
app :: SVT.Application
app = SVT.serve RT.gallery RT.endpoints

---------------------------------------------------------------------------------------------------
runServer :: Maybe Int -> IO ()
runServer port = do
    let serverPort = fromJust $ port <|> Just 8080 -- TODO(rodrigo): falback port to an environment variable
    putStrLn("Running server at port " ++ (show serverPort) ++ "\nPress CTRL-C to terminate")
    run serverPort app
