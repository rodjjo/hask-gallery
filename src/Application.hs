{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Application (
        runServer
    ) where

import qualified Routes as RT
import qualified Views.Base as VB

import Control.Applicative (Alternative((<|>)))
import Control.Monad.Trans.Reader  (runReaderT)
import Data.List ((++), elem)
import Data.Int (Int)
import Data.Bool (Bool(..))
import Data.String (String)
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


bindAllOptions = ["True", "true", "yes", "Yes"]

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
bindAnyFromEnv :: Bool -> IO Bool
bindAnyFromEnv def = do
    mhost <- lookupEnv "BIND_ANY_ADDRESS"
    if mhost == Nothing
        then return def
        else return $ (fromJust mhost) `elem` bindAllOptions

---------------------------------------------------------------------------------------------------
runServer :: Maybe String -> Maybe Int -> IO ()
runServer bindAny port = do
    let shouldBindAll = (if bindAny == Nothing then Nothing else Just $ (fromJust bindAny) `elem` bindAllOptions)
    state <- VB.loadInitialState
    envPort <- portFromEnv 8080
    envHost <- bindAnyFromEnv False
    let serverPort = fromJust $ port <|> Just envPort
    let bindAny = fromJust $ shouldBindAll <|> Just envHost
    let settings = setPort serverPort  $ setHost (if bindAny then "*" else "127.0.0.1") defaultSettings
    putStrLn("Running server at " ++ (if bindAny then "*" else "127.0.0.1") ++ ":" ++ (show serverPort) ++ "\nPress CTRL-C to terminate")
    runSettings settings $ app $ VB.State state
