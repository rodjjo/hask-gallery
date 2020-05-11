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
import Text.Show (Show, show)

---------------------------------------------------------------------------------------------------
data Options =
    OptionCommand |
    OptionPort |
    OptionText
    deriving (Ord, Eq, Show)

commands :: [String]
commands = ["help", "refresh", "gal-title", "gal-videos-path"]

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
            PA.argData = PA.argDataOptional "command" PA.ArgtypeString,
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
setSetting cmdOption text = do
    let option = drop 4 cmdOption
    if option `elem` SM.validOptions
        then do
            settings <- SM.load
            SM.save $ (SM.optionSetter $ option) settings text
            newSettings <- SM.load
            putStrLn ("Successfuly configured\nCurrent settings:\n" ++ show newSettings)
        else do
            putStrLn ("Invalid command " ++ option)

---------------------------------------------------------------------------------------------------
exec :: (Maybe Int -> IO ()) -> IO ()
exec runServer = do
    args <- PA.parseArgsIO (PA.ArgsParseControl PA.ArgsComplete PA.ArgsSoftDash) argd
    settings <- SM.load
    let command = fromJust $ PA.getArgString args OptionCommand <|> Just ""
    let text = fromJust $ PA.getArgString args OptionText <|> Just ""
    let port = PA.getArgInt args OptionPort
    case (adaptCommand command) of
        "" -> runServer port
        "gal-config" -> setSetting command text
        "refresh" -> VM.updateCache (SM.getVideoGalleryPath settings)
        "help" -> putStrLn (PA.argsUsage args)
        _ -> putStrLn ("An unknow command was entered: " ++ command ++ "\n" ++ (PA.argsUsage args))
