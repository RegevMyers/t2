module Syn (
    Expression,
    syn
) where

import Internal.Ast as Ast (Expression(..))
import Internal.Parsers
import Internal.Parser as Parser (Parser(run))

import Lex (Token)

import Data.Text (Text)

syn :: [Token] -> Either Text Expression
syn tokens =
    case Parser.run parseExpression tokens of
        Just (expression, _) -> Right expression
        Nothing -> Left $ "sad... :("