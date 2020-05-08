{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Application (runServer)
import qualified Management.Command as MC

import System.IO (IO)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

---------------------------------------------------------------------------------------------------
main :: IO ()
main = do
    setLocaleEncoding utf8
    MC.exec runServer
