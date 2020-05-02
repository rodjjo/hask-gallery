{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Gallery where

import Data.Aeson
import Data.Int (Int)
import Data.Proxy
import Data.Text  (Text)
import Data.Eq (Eq)
import GHC.Generics (Generic)
import Servant.API
import Text.Read (Read)
import Text.Show (Show)

data User = User
    { name :: Text
    , age  :: Int
    } deriving (Eq, Show, Read, Generic)
instance FromJSON User
instance ToJSON User

type Gallery  = Get '[PlainText] Text
            :<|> "user" :> Capture "name" Text :> Capture "age" Int :> Get '[JSON] User

gallery :: Proxy Gallery
gallery = Proxy
