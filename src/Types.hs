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

-- representation for the Account record with identificator as Integer field
newtype Account = Account Integer deriving (Eq, Show, Num, Ord)

-- model of Transaction, with source and destination accounts + desired money amount to transfer
data Transaction = Transaction
  { _from :: Account,
    _to :: Account,
    _amount :: Integer
  }
  deriving (Eq, Show, Ord)

-- Block representation which contains list of elements
-- plus some extensions for processing
newtype BlockF a = Block (V.Vector a) deriving (Eq, Show, Foldable, Traversable, Functor, Monoid)

-- wraooer ffor block type, which contains list of transactions
type Block = BlockF Transaction

-- typealias for the blockchain hash
type HaskelchainHash = Digest SHA1

-- block header, which attached to each block. contains useful metadata to work with
data BlockHeader = BlockHeader
  { _miner :: Account,
    _parentHash :: HaskelchainHash,
    _nonce :: Integer,
    _minedAt :: POSIXTime
  }
  deriving (Eq, Show)

-- basic types for representation of blockchain elements
-- Genesis is an empty block, Node contains metadata and some value
data MerkleF a
  = Genesis
  | Node BlockHeader a
  deriving (Eq, Show, Functor, Traversable, Foldable)

-- common type for the whole blockchain
-- contains Cofree because of possiblity to wrap elements
-- Cofree helps to create chain of elements, and then represent them as a blockchain instance
type Blockchain = Cofree MerkleF Block

-- instance of Semigroup for handling future operations
deriving instance Semigroup (BlockF a)
