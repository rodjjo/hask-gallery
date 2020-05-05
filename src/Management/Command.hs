{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Management.Command (
        exec
    ) where

import qualified Models.Video as VM
import qualified Models.Settings as SM

import Control.Applicative (Alternative((<|>)))
import Data.Eq (Eq)
import Data.Int (Int)
import Data.List (elem, foldl, drop, take, (++))
import Data.Maybe
import Data.Ord (Ord)
import Data.String (String)
import Prelude (otherwise, return, (==), (||), ($))
import qualified System.Console.ParseArgs as PA
import System.IO (IO, putStrLn)
import Text.Show (Show)

---------------------------------------------------------------------------------------------------
data Options =
    OptionCommand |
    OptionPort |
    OptionText
    deriving (Ord, Eq, Show)

commands :: [String]
commands = ["runserver", "refresh", "gal-title", "gal-videos-path"]

argd :: [ PA.Arg Options ]
argd = [
        PA.Arg {
            PA.argIndex = OptionPort,
            PA.argName = Just "port",
            PA.argAbbr = Just 'p',
            PA.argData = PA.argDataOptional "port" PA.ArgtypeInt,
            PA.argDesc = "The port to listen at. ex. 8080"},
        PA.Arg {
            PA.argIndex = OptionCommand,
            PA.argName = Nothing,
            PA.argAbbr = Nothing,
            PA.argData = PA.argDataRequired "command" PA.ArgtypeString,
            PA.argDesc = "Command: " ++ foldl (\a b -> a ++ b ++ "|") "|" commands },
        PA.Arg {
            PA.argIndex = OptionText,
            PA.argName = Nothing,
            PA.argAbbr = Nothing,
            PA.argData = PA.argDataDefaulted "text" PA.ArgtypeString "",
            PA.argDesc = "Text parameter for gal-title or gal-*-path command"}
    ]

---------------------------------------------------------------------------------------------------
adaptCommand :: String -> String
adaptCommand text
    | take 4 text == "gal-" = "gal-config"
    | otherwise = text

---------------------------------------------------------------------------------------------------
setSetting :: String -> String -> IO ()
setSetting option text = do
    settings <- SM.load
    SM.save $ (SM.optionSetter $ drop 4 option) settings text

---------------------------------------------------------------------------------------------------
exec :: (Maybe Int -> IO ()) -> IO ()
exec runServer = do
    args <- PA.parseArgsIO (PA.ArgsParseControl PA.ArgsComplete PA.ArgsSoftDash) argd
    let command = fromJust $ PA.getArgString args OptionCommand <|> Just ""
    let text = fromJust $ PA.getArgString args OptionText <|> Just ""
    let port = PA.getArgInt args OptionPort
    case (adaptCommand command) of
        "runserver" -> runServer port
        "gal-config" -> setSetting command text
        "refresh" -> VM.updateCache
        _ -> putStrLn ("An unknow command was entered: " ++ command ++ "\n" ++ (PA.argsUsage args))
