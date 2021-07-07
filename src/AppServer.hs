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
import System.IO.Unsafe
import Types

data BalanceList = BalanceList
  { account :: Integer,
    amount :: Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON BalanceList

data BlockMineInfo = BlockMineInfo
  { fileName :: String,
    accountValue :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON BlockMineInfo

instance ToJSON BlockMineInfo

data BlockMineMessage = BlockMineMessage
  {message :: String}
  deriving (Eq, Show, Generic)

instance ToJSON BlockMineMessage

type API =
  "listBalancesRoute"
    :> Capture "file_name" String
    :> Get '[JSON] [BalanceList]
    :<|> "mineBlocksRoute"
      :> ReqBody '[JSON] BlockMineInfo
      :> Post '[JSON] BlockMineMessage
    :<|> "createChainFileRoute"
      :> Capture "file_name" String
      :> Get '[JSON] BlockMineMessage

convert :: (Account, Integer) -> BalanceList
convert (Account accValue, amountValue) = BalanceList accValue amountValue

appServer :: Server API
{-# NOINLINE appServer #-}
appServer = listBalancesRoute :<|> mineBlocksRoute :<|> createChainFileRoute
  where
    listBalancesRoute :: String -> Handler [BalanceList]
    listBalancesRoute filePath = return result
      where
        accountWithBalances = unsafePerformIO (listBalances filePath)
        result = map convert accountWithBalances

    mineBlocksRoute :: BlockMineInfo -> Handler BlockMineMessage
    mineBlocksRoute (BlockMineInfo _fileName _accountValue) = return result
      where
        minedMessage = unsafePerformIO (mineAndSaveBlock _fileName _accountValue)
        result = BlockMineMessage minedMessage

    createChainFileRoute :: String -> Handler BlockMineMessage
    createChainFileRoute fileChain = return result
      where
        unitResult = unsafePerformIO (createEmptyChainFile fileChain)
        result = BlockMineMessage ("file chain with path" ++ fileChain ++ "created")

api :: Proxy API
api = Proxy

app :: Application
app = serve api appServer
