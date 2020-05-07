{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Models.Base (
     loadModel
    ,loadModelList
    ,saveModel
    ,saveModelList
    ,shuffleModelList
    ) where

import Utils (
         baseDir
        ,baseFilePath
        ,readContents
        ,writeContents
        ,nextRandomNumber
        ,shuffleList
        ,randomList
    )

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Int (Int)
import Data.List (head, zip, (++))
import Data.Maybe
import Data.Ord (Ord)
import Data.String (String)
import System.IO (IO)
import Prelude (compare, return, ($))

-- TODO(rodrigo): make loadModel to able to load single element or list
---------------------------------------------------------------------------------------------------
loadModel :: (FromJSON a) => String -> a -> IO a
loadModel filename def = do
    contents <- readContents filename
    case (decode (Char8.pack contents)) of
        Just a -> return a
        Nothing -> return def

---------------------------------------------------------------------------------------------------
loadModelList :: (FromJSON a) => String -> IO [a]
loadModelList filename = do
    contents <- readContents filename
    case (decode (Char8.pack contents)) of
        Just a -> return a
        Nothing -> return []

---------------------------------------------------------------------------------------------------
saveModel :: (ToJSON a) => String -> a -> IO ()
saveModel filename value = do
    writeContents filename $ Char8.unpack $ encode value

---------------------------------------------------------------------------------------------------
saveModelList :: (ToJSON a) => String -> [a] -> IO ()
saveModelList filename value = do
    writeContents filename $ Char8.unpack $ encode value

---------------------------------------------------------------------------------------------------
shuffleModelList :: (ToJSON a) => [a] -> Int -> [a]
shuffleModelList models seed =
    [ item | (_, item) <- shuffleList [ (pos, a) | (pos, a) <- zip (randomList [seed]) models ] ]
