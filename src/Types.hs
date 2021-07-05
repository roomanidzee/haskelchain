{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Control.Comonad.Cofree
import Control.Concurrent.STM.TVar
import Crypto.Hash
import qualified Data.Set as S
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import Protolude

newtype Account = Account Integer deriving (Eq, Show, Num, Ord)

data Transaction = Transaction
  { _from :: Account,
    _to :: Account,
    _amount :: Integer
  }
  deriving (Eq, Show, Ord)

newtype BlockF a = Block (V.Vector a) deriving (Eq, Show, Foldable, Traversable, Functor, Monoid)

type Block = BlockF Transaction

type HaskoinHash = Digest SHA1

data BlockHeader = BlockHeader
  { _miner :: Account,
    _parentHash :: HaskoinHash,
    _nonce :: Integer,
    _minedAt :: POSIXTime
  }
  deriving (Eq, Show)

data MerkleF a
  = Genesis
  | Node BlockHeader a
  deriving (Eq, Show, Functor, Traversable, Foldable)

type Blockchain = Cofree MerkleF Block

deriving instance Semigroup (BlockF a)

data ServerState = ServerState
  { _longestChain :: TVar Blockchain,
    _supernodes :: TVar (S.Set Supernode),
    _txnPool :: TVar (S.Set Transaction)
  }

newtype SeedServer = SeedServer Text deriving (Eq, Show, Generic)

data Supernode = Supernode
  { _nodeName :: Text,
    _nodeHost :: Text
  }
  deriving (Eq, Show, Generic, Ord)
