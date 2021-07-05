{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module BlockchainService where

import Data.Binary (Binary, decodeFile, encode, encodeFile)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import Mining (balances, makeGenesis, mineOn)
import Protolude
  ( Char,
    FilePath,
    IO,
    Integer,
    Monad (return),
    forever,
    print,
    ($),
    (++),
  )
import Serialization ()
import System.Directory (copyFile, doesFileExist)
import Types
import Prelude (read)

listBalances :: FilePath -> IO [(Account, Integer)]
listBalances fileName = do
  chain <- decodeFile fileName :: IO Blockchain
  return (M.toAscList $ balances chain)

loadOrCreateFileChain :: Binary a => FilePath -> IO a -> IO a
loadOrCreateFileChain fileName initDir = do
  exists <- doesFileExist fileName
  if exists
    then decodeFile fileName
    else do
      x <- initDir
      encodeFile fileName x
      return x

-- TODO think about return type
mineAndSaveBlock :: FilePath -> [Char] -> IO b
mineAndSaveBlock fileName accountStorage = do
  let swapFile = fileName ++ ".tmp"

  let txnPool = return []

  let account = Account $ read accountStorage

  forever $ do
    chain <- loadOrCreateFileChain fileName makeGenesis :: IO Blockchain
    newChain <- mineOn txnPool account chain
    encodeFile swapFile newChain
    copyFile swapFile fileName
    print "Block mined and saved!"

-- TODO think about return type
createEmptyChainFile :: FilePath -> IO ()
createEmptyChainFile fileName = do
  chain <- makeGenesis
  BSL.writeFile fileName $ encode chain