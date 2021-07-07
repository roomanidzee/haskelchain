{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module AppServer where

import Data.Aeson (ToJSON)
import Data.Time.Calendar (Day, fromGregorian)
import GHC.Generics (Generic)
import Prelude.Compat (Eq, Int, Monad (return), Show, String)
import Servant
  ( Application,
    Get,
    JSON,
    Proxy (..),
    Server,
    serve,
    type (:>),
  )
import Prelude ()

data User = User
  { name :: String,
    age :: Int,
    email :: String,
    registration_date :: Day
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

type UserAPI = "users" :> Get '[JSON] [User]

users1 :: [User]
users1 =
  [ User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1),
    User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)
  ]

server :: Server UserAPI
server = return users1

userAPI :: Proxy UserAPI
userAPI = Proxy

app :: Application
app = serve userAPI server
