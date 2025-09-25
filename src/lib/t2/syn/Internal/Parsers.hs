{-# LANGUAGE OverloadedStrings #-}

module Internal.Parsers (
    parseExpression
) where

import Internal.Ast as Ast
import Internal.Parser (Parser(..))
import Internal.Guards

import qualified Lex (Token(..))

import Utils.Alternative (try)

import Data.Text as Text (Text, unpack)
import Control.Applicative ((<|>), optional)

parseExpression :: Parser Ast.Expression
parseExpression = do
    guardNotEmpty
    parseBinaryExpression 0 <|> parseInteger

parseTerm :: Parser Ast.Expression
parseTerm = do
    guardNotEmpty
    parseInteger

parseInteger :: Parser Ast.Expression
parseInteger = do
    guardNotEmpty
    Parser $ parse
    where
        parse (Lex.Integer integerAsText : rest) =
            Just (Ast.Integer $ readInteger integerAsText, rest)
            where
                readInteger text = (read . Text.unpack) text
        parse _ = Nothing

-- 1   +   1   *   1   +   4   +   4   +   4
--   1   1+  2   2+  1   1+  1   1+  1   1+

parseBinaryExpression :: Int -> Parser Ast.Expression
parseBinaryExpression minPower = do
    guardNotEmpty
    term <- parseTerm
    try (addTail minPower) term
    where
        addTail minPower' lhs = do
            (op@(Ast.Binop _ opPower associativity), rhs) <- parseBinaryExpressionTail minPower'
            let binexpr = Ast.Binexpr op lhs rhs
            case associativity of
                LeftAssociative -> try (addTail opPower) binexpr
                _               -> pure binexpr

parseBinaryExpressionTail :: Int -> Parser (Ast.Binop, Ast.Expression)
parseBinaryExpressionTail minPower = do
    guardCount (>= 2)
    op@(Ast.Binop _ opPower associativity) <- parseBinaryOperator
    assert $ opPower >= minPower
    -- assert $ case associativity of
    --     LeftAssociative  -> opPower >  minPower
    --     RightAssociative -> opPower >= minPower
    rhs <- parseBinaryExpression (if associativity == LeftAssociative then opPower + 1 else opPower)
    return $ (op, rhs)

parseBinaryOperator :: Parser Ast.Binop
parseBinaryOperator = do
    guardNotEmpty
    Parser $ \case
        (Lex.Binop op : rest) -> (, rest) <$> lookup op opTable
        _                     -> Nothing

opTable :: [(Text, Ast.Binop)]
opTable = [
        ("+", Ast.Binop Plus   1 LeftAssociative),
        ("-", Ast.Binop Minus  1 RightAssociative),
        ("*", Ast.Binop Times  2 LeftAssociative),
        ("/", Ast.Binop Divide 2 Unassociative)
    ]