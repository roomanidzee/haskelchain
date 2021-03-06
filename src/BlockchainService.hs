{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BlockchainService where

import Data.Binary (Binary, decodeFile, encode, encodeFile)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import Data.String (IsString)
import Mining (balances, makeGenesis, mineOn)
import Protolude
  ( Char,
    FilePath,
    Integer,
    Monad (return),
    ($),
    (++),
  )
import Serialization ()
import System.Directory (copyFile, doesFileExist)
import Types (Account (..), Blockchain)
import Prelude (IO, read)

-- listing of balances from input file path
listBalances :: FilePath -> IO [(Account, Integer)]
listBalances fileName = do
  chain <- decodeFile fileName :: IO Blockchain
  return (M.toAscList $ balances chain)

-- load chain file or create it with input block
loadOrCreateFileChain :: Binary b => FilePath -> IO b -> IO b
loadOrCreateFileChain fileName initDir = do
  exists <- doesFileExist fileName
  if exists
    then decodeFile fileName
    else do
      x <- initDir
      encodeFile fileName x
      return x

-- mining of block and saving it to input file
mineAndSaveBlock :: IsString b => FilePath -> [Char] -> IO b
mineAndSaveBlock fileName accountStorage = do
  let swapFile = fileName ++ ".tmp"

  let txnPool = return []

  let account = Account $ read accountStorage

  chain <- loadOrCreateFileChain fileName makeGenesis :: IO Blockchain
  newChain <- mineOn txnPool account chain
  encodeFile swapFile newChain
  copyFile swapFile fileName
  return "Block mined and saved!"

--creation of new chain file in particular path
createEmptyChainFile :: FilePath -> IO ()
createEmptyChainFile fileName = do
  chain <- makeGenesis
  BSL.writeFile fileName $ encode chain