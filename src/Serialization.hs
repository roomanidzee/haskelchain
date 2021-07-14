{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Serialization where

import Control.Comonad.Cofree (Cofree)
import Crypto.Hash (digestFromByteString)
import Data.Binary (Binary (get, put), Get)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import Data.Time.Clock ()
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Vector.Binary ()
import GHC.Generics (Generic)
import Types
  ( Account (..),
    BlockF (..),
    BlockHeader (BlockHeader),
    HaskelchainHash,
    MerkleF (..),
    Transaction (Transaction),
  )

{-

This module contains useful instances for serializing and deserializing
blockchain models with binary data format

-}

instance (Binary (f (Cofree f a)), Binary a) => Binary (Cofree f a)

instance (Binary a) => Binary (MerkleF a)

instance Binary BlockHeader

instance Binary Account

instance Binary Transaction

instance Binary (BlockF Transaction)

instance Binary POSIXTime where
  get = fromInteger <$> (get :: Get Integer)
  put x = put (round x :: Integer)

deriving instance Generic (MerkleF a)

deriving instance Generic (BlockF Transaction)

deriving instance Generic BlockHeader

deriving instance Generic Account

deriving instance Generic Transaction

instance Binary HaskelchainHash where
  get = do
    mDigest <- digestFromByteString <$> (get :: Get BS.ByteString)
    case mDigest of
      Nothing -> fail "Not a valid digest"
      Just digest -> return digest
  put digest = put (convert digest :: BS.ByteString)