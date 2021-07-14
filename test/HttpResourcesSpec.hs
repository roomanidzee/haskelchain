{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HttpResourcesSpec where

import AppServer (app)
import Test.Hspec
import Test.Hspec.Wai

spec :: Spec
spec = with (return app) $ do
  describe "Tests for Haskelchain HTTP Resources" $ do
    it "create empty file chain" $ do
      get "/createChainFileRoute?file_name=genesis_chain1_file" `shouldRespondWith` 200

    it "return balances for chain" $ do
      get "/listBalancesRoute?file_name=genesis_chain_file" `shouldRespondWith` 200

    it "perform block mining" $ do
      get "/mineBlocksRoute?file_name=genesis_chain_file&account_value=1" `shouldRespondWith` 200