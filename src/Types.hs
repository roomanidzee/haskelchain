{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import Control.Comonad.Cofree (Cofree)
import Crypto.Hash (Digest, SHA1)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Vector as V
import Protolude
  ( Eq,
    Foldable,
    Functor,
    Generic,
    Integer,
    Monoid,
    Num,
    Ord,
    Semigroup,
    Show,
    Text,
    Traversable,
  )

newtype Account = Account Integer deriving (Eq, Show, Num, Ord)

data Transaction = Transaction
  { _from :: Account,
    _to :: Account,
    _amount :: Integer
  }
  deriving (Eq, Show, Ord)

newtype BlockF a = Block (V.Vector a) deriving (Eq, Show, Foldable, Traversable, Functor, Monoid)

type Block = BlockF Transaction

type HaskelchainHash = Digest SHA1

data BlockHeader = BlockHeader
  { _miner :: Account,
    _parentHash :: HaskelchainHash,
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
