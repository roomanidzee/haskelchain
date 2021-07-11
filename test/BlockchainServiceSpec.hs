module BlockchainServiceSpec where

import BlockchainService
  ( createEmptyChainFile,
    listBalances,
    loadOrCreateFileChain,
    mineAndSaveBlock,
  )
import qualified Data.Map as M
import Mining (headers, makeGenesis)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

spec :: Spec
spec =
  describe "Tests for BlockchainService module" $ do
    it "create empty file with genesis chain" $ do
      let unitResult = unsafePerformIO (createEmptyChainFile "genesis_chain_file")
      2 + 2 == 4 `shouldBe` True

    it "load created file chain" $ do
      let genesisChain = makeGenesis

      let existedChain = unsafePerformIO (loadOrCreateFileChain "genesis_chain_file" genesisChain)

      let chainHeaders = headers existedChain

      not (null chainHeaders) `shouldBe` False

    it "mine and save block" $ do
      let testAccount = "5"

      let fileName = "mined_file"

      let blockMessage = unsafePerformIO (mineAndSaveBlock fileName testAccount)

      blockMessage == "Block mined and saved!" `shouldBe` True

    it "list balances from input file chain" $ do
      let fileName = "mined_file"

      let balancesInfo = M.fromList (unsafePerformIO (listBalances fileName))

      (balancesInfo M.! 5) > 1000 `shouldBe` True
