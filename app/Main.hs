{-# LANGUAGE OverloadedStrings #-}
module Main where

import Gallery
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API

server :: Server Gallery
server = hello :<|> user
    where
        hello = return "Hello world"
        user n a = return $ User n a

app :: Application
app = serve gallery server

main :: IO ()
main = do
    run 8080 app
