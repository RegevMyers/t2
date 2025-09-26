module Syn (
    Expression(..),
    BinopType(..),
    BinopPower,
    BinopAssociativity(..),
    Binop(..),
    syn
) where

import Internal.Ast as Ast (Expression(..), BinopType(..), BinopPower, BinopAssociativity(..), Binop(..))
import Internal.Parsers
import Internal.Parser as Parser (Parser(run))

import Lex (Token)

import Data.Text (Text, pack)

syn :: [Token] -> Either Text [Expression]
syn tokens =
    case run parseExpressions tokens of
        Just (expressions, []) -> Right expressions
        Just (_, rest)         -> Left $ (pack . show) rest
        _                      -> Left "oopsie woopsie... :("
