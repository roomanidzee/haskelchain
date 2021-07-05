{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Serialization where

import Control.Comonad.Cofree
import Crypto.Hash
import Data.Binary
import Data.Binary.Get
import Data.ByteArray
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Vector.Binary
import GHC.Generics
import Types

instance (Binary (f (Cofree f a)), Binary a) => Binary (Cofree f a)

instance (Binary a) => Binary (MerkleF a)

instance Binary BlockHeader

instance Binary Account

instance Binary Transaction

instance Binary (BlockF Transaction)

instance Binary POSIXTime where
  get = fromInteger <$> (get :: Get Integer)
  put x = put $ (round x :: Integer)

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
  put digest = put $ (convert digest :: BS.ByteString)