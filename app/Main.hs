{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Gallery
import qualified Models.Video as VM
import qualified Models.Settings as SM
import Control.Applicative (Alternative((<|>)))
import Data.Eq (Eq)
import Data.Int (Int)
import Data.List ((++), elem, drop, take)
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
            argDesc = "Command to run: runserver|refresh|gal-title|gal-videos-path"},
        Arg { argIndex = OptionText,
            argName = Nothing,
            argAbbr = Nothing,
            argData = argDataDefaulted "text" ArgtypeString "",
            argDesc = "Text parameter for gal-title or gal-*-path command"}
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
    SM.save $ (SM.optionSetter $ drop 4 option) settings text

---------------------------------------------------------------------------------------------------
adaptCommand :: String -> String
adaptCommand text
    | take 4 text == "gal-" = "gal-config"
    | otherwise = text

---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- parseArgsIO (ArgsParseControl ArgsComplete ArgsSoftDash) argd
    let command = fromJust $ getArgString args OptionCommand <|> Just ""
    let text = fromJust $ getArgString args OptionText <|> Just ""
    let port = fromJust $ getArgInt args OptionPort  <|> Just (8080 ::Int)
    case (adaptCommand command) of
        "runserver" -> runserver port
        "refresh" -> VM.updateCache
        "gal-config" -> setSetting command text
        _ -> putStrLn("Unknow command " ++ command ++ "\n" ++ argsUsage args)
