{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Gallery where

import           Data.Aeson
import           Data.Proxy
import           Data.Text  (Text)
import qualified Data.Text  as T
import           GHC.Generics
import           Servant.API

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
