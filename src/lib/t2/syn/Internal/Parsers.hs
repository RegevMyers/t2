{-# LANGUAGE OverloadedStrings #-}

module Internal.Parsers (
    parseExpression
) where

import Internal.Ast as Ast (Expression(..), BinopType(..))
import Internal.Parser (Parser(..))
import Internal.Guards

import Lex (Token(..))

import Data.Text as Text (Text, unpack)
import Control.Applicative ((<|>))

parseExpression :: Parser Expression
parseExpression = do
    guardNotEmpty
    parseBinaryExpression <|> parseInteger

parseBinaryExpression :: Parser Expression
parseBinaryExpression = do
    guardCount (>= 3)
    (flip Ast.Binop) <$>
        parseTerm <*> parseBinop <*> parseExpression

parseTerm :: Parser Expression
parseTerm = do
    guardNotEmpty
    parseInteger

parseInteger :: Parser Expression
parseInteger = do
    guardNotEmpty
    Parser $ parse
    where
        parse (Lex.Integer integerAsText : rest) =
            Just (Ast.Integer $ readInteger integerAsText, rest)
            where
                readInteger text = (read . Text.unpack) text
        parse _ = Nothing

parseBinop :: Parser BinopType
parseBinop = do
    guardNotEmpty
    Parser $ \case
        (Lex.Binop op : rest) -> (, rest) <$> lookup op opTable
        _                     -> Nothing

opTable :: [(Text, Ast.BinopType)]
opTable = [
        ("+", Ast.Plus),
        ("-", Ast.Minus),
        ("*", Ast.Times),
        ("/", Ast.Divide)
    ]