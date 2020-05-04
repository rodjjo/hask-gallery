{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Gallery
import qualified Management.Command as MC
import Control.Applicative (Alternative((<|>)))
import Data.Int (Int)
import Data.List ((++), drop)
import Data.Maybe
import Data.Ord (Ord)
import Data.String (String)
import Network.Wai
import Network.Wai.Handler.Warp
import Prelude (otherwise, return, (==), (||), ($))
import Servant
import Servant.API
import System.Console.ParseArgs
import System.IO (IO, putStrLn)
import Text.Show (show)

---------------------------------------------------------------------------------------------------
server :: Server Gallery
server = hello :<|> user
    where
        hello = return "Hello world"
        user n a = return $ User n a

---------------------------------------------------------------------------------------------------
app :: Application
app = serve gallery server

---------------------------------------------------------------------------------------------------
runServer :: Maybe Int -> IO ()
runServer port = do
    let serverPort = fromJust $ port <|> Just 8080 -- TODO(rodrigo): falback port to an environment variable
    putStrLn("Running server at port " ++ (show serverPort) ++ "\nPress CTRL-C to terminate")
    run serverPort app

---------------------------------------------------------------------------------------------------
main :: IO ()
main = MC.exec runServer
