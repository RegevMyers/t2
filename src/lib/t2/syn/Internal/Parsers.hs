{-# LANGUAGE OverloadedStrings #-}

module Internal.Parsers (
    parseExpression
) where

import Internal.Ast as Ast
import Internal.Parser (Parser(..))
import Internal.Guards

import qualified Lex (Token(..))

import Data.Text as Text (Text, unpack)
import Control.Applicative ((<|>))

parseExpression :: Parser Ast.Expression
parseExpression = do
    guardNotEmpty
    parseBinaryExpression 0 <|> parseInteger

-- 1   +   1   *   1   +   4   +   4   +   4
--   1   1+  2   2+  1   1+  1   1+  1   1+

parseBinaryExpression :: Int -> Parser Ast.Expression
parseBinaryExpression minPower = do
    guardNotEmpty
    Parser $ \tokens -> do
        (lhs, tokens') <- run parseTerm tokens
        return $ case run parseOpAndRhs tokens' of
            Just ((op, rhs), rest) -> ((Ast.Binexpr op lhs rhs), rest)
            Nothing                -> (lhs, tokens')
            where
                parseOpAndRhs = do
                    guardCount (>= 2)
                    op@(Ast.Binop _ opPower _) <- parseBinaryOperator
                    assert $ opPower >= minPower
                    rhs <- parseBinaryExpression opPower
                    return $ (op, rhs)

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

parseBinaryOperator :: Parser Ast.Binop
parseBinaryOperator = do
    guardNotEmpty
    Parser $ \case
        (Lex.Binop op : rest) -> (, rest) <$> lookup op opTable
        _                     -> Nothing

opTable :: [(Text, Ast.Binop)]
opTable = [
        ("+", Ast.Binop Plus   1 LeftAssociative),
        ("-", Ast.Binop Minus  1 LeftAssociative),
        ("*", Ast.Binop Times  2 LeftAssociative),
        ("/", Ast.Binop Divide 2 LeftAssociative)
    ]