module SynSpec (spec) where

import Prelude hiding (lex)

import Lex (lex)
import Syn as Ast

import Test.Hspec

import Control.Monad ((>=>))

spec :: Spec
spec = describe "Spec" $ do
    describe "syn" $ do
        it "parses an integer" $ do
            (lex >=> syn) "1" `shouldBe` Right [Ast.Integer 1]
        it "parses a simple math expression" $ do
            (lex >=> syn) "1 + 1" `shouldBe` Right [Ast.Binexpr (Ast.Binop Plus 1 LeftAssociative) (Ast.Integer 1) (Ast.Integer 1)]
        context "associates correctly" $ do
            it "left associates" $ do
                (lex >=> syn) "1 + 2 + 3" `shouldBe` Right [
                        Ast.Binexpr (Ast.Binop Plus 1 LeftAssociative)
                            (Ast.Binexpr (Ast.Binop Plus 1 LeftAssociative)
                                (Ast.Integer 1)
                                (Ast.Integer 2))
                            (Ast.Integer 3)
                    ]
            it "right associates" $ do
                (lex >=> syn) "1 ^ 2 ^ 3" `shouldBe` Right [
                        Ast.Binexpr (Ast.Binop Exp 3 RightAssociative)
                            (Ast.Integer 1)
                            (Ast.Binexpr (Ast.Binop Exp 3 RightAssociative)
                                (Ast.Integer 2)
                                (Ast.Integer 3))
                    ]
            it "respects parens" $ do
                (lex >=> syn) "1 * (2 * 3)" `shouldBe` Right [
                        Ast.Binexpr (Ast.Binop Times 2 LeftAssociative)
                            (Ast.Integer 1)
                            (Ast.Binexpr (Ast.Binop Times 2 LeftAssociative)
                                (Ast.Integer 2)
                                (Ast.Integer 3))
                    ]
