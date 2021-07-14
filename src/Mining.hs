{-# LANGUAGE NoImplicitPrelude #-}

module Mining where

import Control.Comonad.Cofree (Cofree ((:<)))
import Crypto.Hash (hashlazy)
import Crypto.Number.Serialize (os2ip)
import Data.Binary (encode)
import qualified Data.Map as M
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as V
import Protolude
  ( Bool (..),
    Eq ((==)),
    Floating (logBase),
    Foldable (length, toList),
    Fractional ((/)),
    Int,
    Integer,
    Maybe (Just, Nothing),
    Monad (return, (>>=)),
    Monoid (mconcat),
    Num ((*), (+), (-)),
    Ord (min, (<), (>=)),
    RealFrac (round),
    filter,
    fromIntegral,
    fromMaybe,
    map,
    print,
    sum,
    tailMay,
    take,
    zipWith,
    ($),
    (++),
  )
import Serialization ()
import Types
  ( Account,
    Block,
    BlockF (Block),
    BlockHeader (..),
    Blockchain,
    HaskelchainHash,
    MerkleF (Genesis, Node),
    Transaction (..),
  )
import Prelude (IO)

-- typealias for the pool of transactions
type TransactionPool = IO [Transaction]

-- limitation for the amount of transactions, which can be performed
globalTransactionLimit :: Int
globalTransactionLimit = 1000

-- how many blocks required for difficulty calculation
numBlocksToCalculateDifficulty :: Int
numBlocksToCalculateDifficulty = 5

-- difficulty of empty blockchain block
genesisBlockDifficulty :: NominalDiffTime
genesisBlockDifficulty = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- target time for difficulty calculation
targetTime :: NominalDiffTime
targetTime = 10

--reward for block calculation
blockReward :: Integer
blockReward = 1000

-- method for adding the block to existed blockchain
addBlock ::
  Block -> -- input block
  BlockHeader -> -- metadata for a new block
  Blockchain -> -- existed chain with blocks
  Blockchain -- new blockchain
addBlock block header chain = block :< Node header chain

-- checks if input blockchain is valid for operations
-- TODO make a better check in future
validateChain :: Blockchain -> Bool
validateChain _ = True

-- checks if input blockchain and transaction are valid for operations
-- TODO make a better check in future
validateTxn :: Blockchain -> Transaction -> Bool
validateTxn _ _ = True

-- method for metadata from input blockchain
headers :: Blockchain -> [BlockHeader]
headers (_ :< Genesis) = []
headers (_ :< Node x next) = x : headers next

-- calculation of balance for each account in blockchain
balances :: Blockchain -> M.Map Account Integer
balances bc =
  let txns = toList $ mconcat $ toList bc
      debits = map (\Transaction {_from = acc, _amount = amount} -> (acc, - amount)) txns
      credits = map (\Transaction {_to = acc, _amount = amount} -> (acc, amount)) txns
      minings = map (\h -> (_miner h, blockReward)) $ headers bc
   in M.fromListWith (+) $ debits ++ credits ++ minings

-- retrieving of valid transactions from blockchain by filtering of input transactions
validTransactions :: Blockchain -> [Transaction] -> [Transaction]
validTransactions bc txns =
  let accounts = balances bc
      validTxn txn = case M.lookup (_from txn) accounts of
        Nothing -> False
        Just balance -> balance >= _amount txn
   in filter validTxn txns

-- creation of empty blockchain block
makeGenesis :: IO Blockchain
makeGenesis = return $ Block (V.fromList []) :< Genesis

safeDiv :: (Fractional a, Eq a) => a -> a -> a
safeDiv n d = n / (if d == 0 then 1 else d)

average :: (Foldable f, Num a, Fractional a, Eq a) => f a -> a
average xs = sum xs `safeDiv` fromIntegral (length xs)

-- flatenning of input blockchain to list of blocks
chains :: Blockchain -> [Blockchain]
chains x@(_ :< Genesis) = [x]
chains x@(_ :< Node _ next) = x : chains next

--calculation of average block time to be produced
blockTimeAverage :: Blockchain -> NominalDiffTime
blockTimeAverage bc = average $ zipWith (-) times (fromMaybe [] $ tailMay times)
  where
    times = take numBlocksToCalculateDifficulty $ map _minedAt $ headers bc

-- desired difficulty for input blockchain
-- BEWARE: O(n * k), where k = numBlocksToCalculateDifficulty
desiredDifficulty :: Blockchain -> Integer
desiredDifficulty x = round $ loop x
  where
    loop (_ :< Genesis) = genesisBlockDifficulty
    loop (_ :< Node _ xs) = oldDifficulty / adjustmentFactor
      where
        oldDifficulty = loop xs
        adjustmentFactor = min 4.0 $ targetTime `safeDiv` blockTimeAverage x

-- just a difficulty for input blockchain
difficulty :: Blockchain -> Integer
difficulty bc = os2ip (hashlazy $ encode bc :: HaskelchainHash)

-- mining of block on transacction pool for particular account and blockchain
mineOn :: TransactionPool -> Account -> Blockchain -> IO Blockchain
mineOn pendingTransactions minerAccount parent = do
  ts <- pendingTransactions
  let tsTemp = validTransactions parent ts
  let tsResult = take globalTransactionLimit tsTemp
  now <- getPOSIXTime
  result <- loop now tsResult 0 (difficulty parent * 4)
  after <- getPOSIXTime
  print $ after - now
  return result
  where
    validChain bc = difficulty bc < desiredDifficulty parent
    loop now ts nonce bestDifficulty = do
      let header =
            BlockHeader
              { _miner = minerAccount,
                _parentHash = hashlazy $ encode parent,
                _nonce = nonce,
                _minedAt = now
              }
          block = Block (V.fromList ts)
          candidate = addBlock block header parent
          log =
            if candidateDifficulty < bestDifficulty
              then do
                print ("New candidate found: ", logBase 10 $ fromIntegral $ desiredDifficulty parent, logBase 10 $ fromIntegral candidateDifficulty)
                return candidateDifficulty
              else return bestDifficulty
            where
              candidateDifficulty = difficulty candidate
      if validChain candidate
        then return candidate
        else log >>= loop now ts (nonce + 1)
