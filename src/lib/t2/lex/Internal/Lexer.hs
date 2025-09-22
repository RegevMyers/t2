module Internal.Lexer (
    Token(..),
    Lexer
) where

import Data.Text (Text)

data Token =
    Integer Text |
    Binop Text
    deriving(Eq, Show)

type Lexer = Text -> Maybe (Token, Text)
