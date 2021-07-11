module MiningSpec where

import Control.Comonad.Cofree (Cofree (..))
import Crypto.Hash
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX
import qualified Data.Vector as V
import Mining (headers, makeGenesis, validateChain, validateTxn)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Types (Account, BlockF (..), BlockHeader (..), Blockchain, MerkleF (Genesis, Node), Transaction (..))

packStr'' :: String -> B.ByteString
packStr'' = encodeUtf8 . T.pack

spec :: Spec
spec =
  describe "Tests for Mining module" $ do
    it "validate input blockchain" $ do
      let testBlock = unsafePerformIO makeGenesis
      validateChain testBlock `shouldBe` True

    it "validate input transaction" $ do
      let testTransaction = Transaction 300 500 100

      let testBlock = unsafePerformIO makeGenesis

      validateTxn testBlock testTransaction `shouldBe` True

    it "retrieve headers from input blockchain" $ do
      let testGenesisBlock = unsafePerformIO makeGenesis

      headers testGenesisBlock `shouldBe` []

      let testString = "test_hash"

      let testBlockHeader =
            BlockHeader
              { _miner = 1,
                _parentHash = hash (packStr'' testString),
                _nonce = 100,
                _minedAt = unsafePerformIO getPOSIXTime
              }

      let genesisBlock = Block (V.fromList [])

      let testTransaction = Transaction 300 500 100

      let testBlock = Block (V.fromList [testTransaction])

      let genesisChain = genesisBlock :< Genesis

      let testChain = testBlock :< Node testBlockHeader genesisChain

      headers testChain `shouldBe` [testBlockHeader]
