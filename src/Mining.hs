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

type TransactionPool = IO [Transaction]

globalTransactionLimit :: Int
globalTransactionLimit = 1000

numBlocksToCalculateDifficulty :: Int
numBlocksToCalculateDifficulty = 5

genesisBlockDifficulty :: NominalDiffTime
genesisBlockDifficulty = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

targetTime :: NominalDiffTime
targetTime = 10

blockReward :: Integer
blockReward = 1000

addBlock :: Block -> BlockHeader -> Blockchain -> Blockchain
addBlock block header chain = block :< Node header chain

-- TODO fix
validateChain :: Blockchain -> Bool
validateChain _ = True

-- TODO fix
validateTxn :: Blockchain -> Transaction -> Bool
validateTxn _ _ = True

headers :: Blockchain -> [BlockHeader]
headers (_ :< Genesis) = []
headers (_ :< Node x next) = x : headers next

balances :: Blockchain -> M.Map Account Integer
balances bc =
  let txns = toList $ mconcat $ toList bc
      debits = map (\Transaction {_from = acc, _amount = amount} -> (acc, - amount)) txns
      credits = map (\Transaction {_to = acc, _amount = amount} -> (acc, amount)) txns
      minings = map (\h -> (_miner h, blockReward)) $ headers bc
   in M.fromListWith (+) $ debits ++ credits ++ minings

validTransactions :: Blockchain -> [Transaction] -> [Transaction]
validTransactions bc txns =
  let accounts = balances bc
      validTxn txn = case M.lookup (_from txn) accounts of
        Nothing -> False
        Just balance -> balance >= _amount txn
   in filter validTxn txns

makeGenesis :: IO Blockchain
makeGenesis = return $ Block (V.fromList []) :< Genesis

safeDiv :: (Fractional a, Eq a) => a -> a -> a
safeDiv n d = n / (if d == 0 then 1 else d)

average :: (Foldable f, Num a, Fractional a, Eq a) => f a -> a
average xs = sum xs `safeDiv` fromIntegral (length xs)

chains :: Blockchain -> [Blockchain]
chains x@(_ :< Genesis) = [x]
chains x@(_ :< Node _ next) = x : chains next

blockTimeAverage :: Blockchain -> NominalDiffTime
blockTimeAverage bc = average $ zipWith (-) times (fromMaybe [] $ tailMay times)
  where
    times = take numBlocksToCalculateDifficulty $ map _minedAt $ headers bc

-- BEWARE: O(n * k), where k = numBlocksToCalculateDifficulty
desiredDifficulty :: Blockchain -> Integer
desiredDifficulty x = round $ loop x
  where
    loop (_ :< Genesis) = genesisBlockDifficulty
    loop (_ :< Node _ xs) = oldDifficulty / adjustmentFactor
      where
        oldDifficulty = loop xs
        adjustmentFactor = min 4.0 $ targetTime `safeDiv` blockTimeAverage x

difficulty :: Blockchain -> Integer
difficulty bc = os2ip (hashlazy $ encode bc :: HaskelchainHash)

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
