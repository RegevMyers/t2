{-# LANGUAGE OverloadedStrings #-}

module LexSpec (spec) where

import qualified Lex

import Test.Hspec

spec :: Spec
spec = describe "Spec" $ do
    describe "lex" $ do
        it "lexes a simple math expression" $ do
            Lex.lex "1 + 1" `shouldBe` Right [(Lex.Integer, "1"), (Lex.Binop, "+"), (Lex.Integer, "1")]
        it "lexes expressions without spaces" $ do
            Lex.lex "1+1" `shouldBe` Right [(Lex.Integer, "1"), (Lex.Binop, "+"), (Lex.Integer, "1")]
        it "fails on unlexable characters" $ do
            Lex.lex "a" `shouldBe` Left "a"
