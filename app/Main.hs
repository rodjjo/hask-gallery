{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Gallery
import qualified VideoModel as VM
import qualified SettingsModel as SM

import Data.Eq (Eq)
import Data.Int (Int)
import Data.List ((++))
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
import Text.Show (Show, show)

---------------------------------------------------------------------------------------------------
data Options =
    OptionCommand |
    OptionPort |
    OptionText
    deriving (Ord, Eq, Show)

argd :: [ Arg Options ]
argd = [
        Arg {argIndex = OptionPort,
            argName = Just "port",
            argAbbr = Just 'p',
            argData = argDataDefaulted "port" ArgtypeInt 8080,
            argDesc = "The port to listen at. ex. 8080"},
        Arg { argIndex = OptionCommand,
            argName = Nothing,
            argAbbr = Nothing,
            argData = argDataRequired "command" ArgtypeString,
            argDesc = "Command to run: runserver|refresh|set-title|set-path|show-configuration"},
        Arg { argIndex = OptionText,
            argName = Nothing,
            argAbbr = Nothing,
            argData = argDataDefaulted "text" ArgtypeString "",
            argDesc = "Text parameter for set-title or set-path command"}
    ]

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
runserver :: Int -> IO ()
runserver port = do
    putStrLn("Running server at port " ++ (show port) ++ "\nPress CTRL-C to terminate")
    run port app

---------------------------------------------------------------------------------------------------
setSetting :: String -> String -> IO ()
setSetting option text = do
    settings <- SM.load
    case () of
        ()  | option == "set-title" -> SM.save $ SM.Settings text $ SM.getVideoGalleryPath settings
            | option == "set-path" -> SM.save $ SM.Settings (SM.getTitle settings) text

---------------------------------------------------------------------------------------------------
defaultPort :: Int
defaultPort = 8080

---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- parseArgsIO (ArgsParseControl ArgsComplete ArgsSoftDash) argd
    let command = case (getArgString args OptionCommand) of
                    Just s -> (s ::String)
                    Nothing -> ("" ::String)
    let text = case (getArgString args OptionText) of
                    Just s -> (s ::String)
                    Nothing -> ("" ::String)
    let port = case (getArgInt args OptionPort) of
                    Just d -> (d ::Int)
                    Nothing -> defaultPort
    case () of
        ()  | command == ("runserver" ::String) -> runserver port
            | command == ("refresh" ::String) -> VM.updateCache
            | command == ("set-title" ::String) || command == ("set-path" ::String) -> setSetting command text
            | otherwise -> putStrLn("Unknow command " ++ command ++ "\n" ++ argsUsage args)
