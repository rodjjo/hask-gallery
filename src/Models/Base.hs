{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Models.Base (
     loadModel
    ,saveModel
    ,shuffleModelList
    ) where

import Utils (
         baseDir
        ,baseFilePath
        ,readContents
        ,writeContents
        ,shuffleList
        ,randomList
    )

import Codec.Compression.Zlib (compress, decompress)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Int (Int)
import Data.List (head, zip, (++))
import Data.Maybe
import Data.Ord (Ord)
import Data.String (String)
import System.IO (IO)
import Prelude (compare, return, ($), (/=))


---------------------------------------------------------------------------------------------------
loadModel :: (FromJSON a) => String -> a -> IO a
loadModel filename def = do
    contents <- readContents filename
    if contents /= ""
        then case (decode $ decompress (Char8.pack contents)) of
                Just a -> return a
                Nothing -> return def
        else return def

---------------------------------------------------------------------------------------------------
saveModel :: (ToJSON a) => String -> a -> IO ()
saveModel filename value = do
    writeContents filename $ Char8.unpack $ compress $ encode value

---------------------------------------------------------------------------------------------------
shuffleModelList :: (ToJSON a) => [a] -> Int -> [a]
shuffleModelList models seed =
    [ item | (_, item) <- shuffleList [ (pos, a) | (pos, a) <- zip (randomList [seed]) models ] ]
