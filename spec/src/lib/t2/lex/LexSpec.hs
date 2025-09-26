module LexSpec (spec) where

import qualified Lex as Tok
import Lex as Lex (lex)

import Test.Hspec

spec :: Spec
spec = describe "Spec" $ do
    describe "lex" $ do
        context "lexes" $ do
            it "Integer" $ do Lex.lex "1"  `shouldBe` Right [Tok.Integer "1", Tok.Eof]
            it "Binop"   $ do Lex.lex "+"  `shouldBe` Right [Tok.Binop "+", Tok.Eof]
            it "Parens"  $ do Lex.lex "()" `shouldBe` Right [Tok.Paren Tok.Left, Tok.Paren Tok.Right, Tok.Eof]
            it "Newline" $ do Lex.lex "\n" `shouldBe` Right [Tok.Newline, Tok.Eof]
            it "Eof"     $ do Lex.lex ""   `shouldBe` Right [Tok.Eof]
        it "lexes a simple math expression" $ do
            Lex.lex "1 + 1" `shouldBe` Right [Tok.Integer "1", Tok.Binop "+", Tok.Integer "1", Tok.Eof]
        it "lexes expressions without spaces" $ do
            Lex.lex "1+1" `shouldBe` Right [Tok.Integer "1", Tok.Binop "+", Tok.Integer "1", Tok.Eof]
        it "fails on unlexable characters" $ do
            Lex.lex "a" `shouldBe` Left "a"
