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

import BlockchainService (createEmptyChainFile, listBalances, mineAndSaveBlock)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant
  ( Application,
    Get,
    Handler,
    JSON,
    Post,
    Proxy (..),
    QueryParam,
    ReqBody,
    Server,
    serve,
    type (:<|>) (..),
    type (:>),
  )
import System.IO.Unsafe (unsafePerformIO)
import Types (Account (..))

data BalanceList = BalanceList
  { account :: Integer,
    amount :: Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON BalanceList

data BlockMineMessage = BlockMineMessage
  {message :: String}
  deriving (Eq, Show, Generic)

instance ToJSON BlockMineMessage

type API =
  "listBalancesRoute"
    :> QueryParam "file_name" String
    :> Get '[JSON] [BalanceList]
    :<|> "mineBlocksRoute"
      :> QueryParam "file_name" String
      :> QueryParam "account_value" String
      :> Get '[JSON] BlockMineMessage
    :<|> "createChainFileRoute"
      :> QueryParam "file_name" String
      :> Get '[JSON] BlockMineMessage

convert :: (Account, Integer) -> BalanceList
convert (Account accValue, amountValue) = BalanceList accValue amountValue

appServer :: Server API
{-# NOINLINE appServer #-}
appServer = listBalancesRoute :<|> mineBlocksRoute :<|> createChainFileRoute
  where
    listBalancesRoute :: Maybe String -> Handler [BalanceList]
    listBalancesRoute inputElem
      | inputElem == Nothing = return [BalanceList 0 0]
      | otherwise = return result
      where
        (Just fileNameValue) = inputElem
        accountWithBalances = unsafePerformIO (listBalances fileNameValue)
        result = map convert accountWithBalances

    mineBlocksRoute :: Maybe String -> Maybe String -> Handler BlockMineMessage
    mineBlocksRoute fileNameOpt accountValueOpt
       | fileNameOpt == Nothing || accountValueOpt == Nothing = return (BlockMineMessage "no such file chain")
       | fileNameOpt == Nothing && accountValueOpt == Nothing = return (BlockMineMessage "no such file chain")
       | otherwise = return result
      where
        (Just _fileName) = fileNameOpt
        (Just _accountValue) = accountValueOpt
        minedMessage = unsafePerformIO (mineAndSaveBlock _fileName _accountValue)
        result = BlockMineMessage minedMessage

    createChainFileRoute :: Maybe String -> Handler BlockMineMessage
    createChainFileRoute inputElem
      | inputElem == Nothing = return (BlockMineMessage "no such file chain")
      | otherwise = return result
      where
        (Just fileNameValue) = inputElem
        unitResult = unsafePerformIO (createEmptyChainFile fileNameValue)
        result = BlockMineMessage ("file chain with path" ++ fileNameValue ++ "created")

api :: Proxy API
api = Proxy

app :: Application
app = serve api appServer
