module MiningSpec where

import Mining (makeGenesis, validateChain, validateTxn)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Types (Transaction (..))

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
