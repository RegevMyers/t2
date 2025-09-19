module Internal.Lexer (
    TokenType(..),
    Token,
    Lexer
) where

import Data.Text (Text)

data TokenType =
    Integer |
    Binop
    deriving(Eq, Show)

type Lexeme = Text

type Token = (TokenType, Lexeme)

type Lexer = Text -> (Maybe Token, Text)
