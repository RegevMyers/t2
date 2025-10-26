{-# LANGUAGE OverloadedStrings #-}

module Internal.Parsers (
    parseExpressions
) where

import Internal.Ast as Ast
import Internal.Parser (Parser(..))
import Internal.Guards

import Lex (Token)
import qualified Lex as Tok (Token(..), ParenType(..))

import Utils.Alternative (try)
import Utils.Function ((<&>))

import Data.Text as Text (Text, unpack)
import Control.Applicative ((<|>))

parseExpressions :: Parser [Expression]
parseExpressions = Parser $ parse
    where
        parse [] = Just ([], [])
        parse tokens = runParser (parseFullExpression <&> (:) <*> parseExpressions) tokens

parseFullExpression :: Parser Expression
parseFullExpression = do
    guardNotEmpty
    parseExpression <* parseExpressionEnd

parseExpression :: Parser Expression
parseExpression = do
    guardNotEmpty
    (parseBinaryExpression 0 <|> parseTerm)

parseExpressionEnd :: Parser ()
parseExpressionEnd = parseToken Tok.Newline <|> parseToken Tok.Eof

parseTerm :: Parser Expression
parseTerm = do
    guardNotEmpty
    parseInteger <|> parseParens

parseInteger :: Parser Expression
parseInteger = do
    guardNotEmpty
    Parser $ parse
    where
        parse (Tok.Integer integerAsText : rest) =
            Just (Ast.Integer $ readInteger integerAsText, rest)
            where
                readInteger text = (read . Text.unpack) text
        parse _ = Nothing

parseParens :: Parser Expression
parseParens = do
    guardCount (>= 3)
    parseToken (Tok.Paren Tok.Left) *> parseExpression <* parseToken (Tok.Paren Tok.Right)

parseBinaryExpression :: BinopPower -> Parser Expression
parseBinaryExpression minPower = do
    guardNotEmpty
    parseTerm >>= try (addBinaryExpressionTail minPower)

addBinaryExpressionTail :: BinopPower -> Expression -> Parser Expression
addBinaryExpressionTail minPower lhs = do
    (op@(Ast.Binop _ power associativity), rhs) <- parseBinaryExpressionTail minPower
    let binexpr = Ast.Binexpr op lhs rhs
    case associativity of
        LeftAssociative -> try (addBinaryExpressionTail power) binexpr
        _               -> pure binexpr

parseBinaryExpressionTail :: BinopPower -> Parser (Binop, Expression)
parseBinaryExpressionTail minPower = do
    guardCount (>= 2)
    op@(Ast.Binop _ power associativity) <- parseBinaryOperator
    guard $ power >= minPower
    rhs <- parseRhs power associativity
    return (op, rhs)
    where
        parseRhs power associativity = case associativity of
            LeftAssociative  -> parseBinaryExpression $ power + 1
            RightAssociative -> parseBinaryExpression power
            Unassociative    -> parseTerm

parseBinaryOperator :: Parser Binop
parseBinaryOperator = do
    guardNotEmpty
    Parser $ \case
        (Tok.Binop op : rest) -> (, rest) <$> lookup op opTable
        _                     -> Nothing

opTable :: [(Text, Ast.Binop)]
opTable = [
        ("+", Ast.Binop Plus   1 LeftAssociative),
        ("-", Ast.Binop Minus  1 LeftAssociative),
        ("*", Ast.Binop Times  2 LeftAssociative),
        ("/", Ast.Binop Divide 2 LeftAssociative),
        ("^", Ast.Binop Exp    3 RightAssociative)
    ]

parseToken :: Token -> Parser ()
parseToken token = do
    guardNotEmpty
    Parser $ parse
    where
        parse (token':rest) = if token' == token then Just ((), rest) else Nothing
        parse _             = Nothing