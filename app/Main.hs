{-# LANGUAGE OverloadedStrings #-}
module Main where

import Gallery
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import System.Console.ParseArgs
import Data.Maybe

data Options =
    OptionCommand |
    OptionPort
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
            argDesc = "Command to run: runserver|refresh"}
    ]

server :: Server Gallery
server = hello :<|> user
    where
        hello = return "Hello world"
        user n a = return $ User n a

app :: Application
app = serve gallery server


runserver :: Int -> IO ()
runserver port = do
    putStrLn("Running server at port " ++ (show port) ++ "\nPress CTRL-C to terminate")
    run port app

refresh :: IO ()
refresh = do
    putStrLn("Did nothing yet")

defaultPort :: Int
defaultPort = 8080

main :: IO ()
main = do
    args <- parseArgsIO (ArgsParseControl ArgsComplete ArgsSoftDash) argd
    let command = case (getArgString args OptionCommand) of
                Just s -> (s ::String)
                Nothing -> ("" ::String)

    let port = case (getArgInt args OptionPort) of
                Just d -> (d ::Int)
                Nothing -> defaultPort
    case () of
        ()  | command == ("runserver" ::String) -> runserver port
            | command == ("refresh" ::String) -> refresh
            | otherwise -> putStrLn("Unknow command " ++ command ++ "\n" ++ argsUsage args)
