{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (spec) where

import Test.Hspec  (describe, it, shouldBe)
import Server      (handleCommand)
import Protocol    (Request (..), Response (..))

spec = describe "Server" $ do
  describe "handleCommand" $ do
    it "returns an InvalidRequest if the request is garbled" $ do
      handleCommand "bogus command" `shouldBe` InvalidRequest

    it "responds to Thing with Ohai" $ do
      handleCommand "Thing" `shouldBe` Ohai
