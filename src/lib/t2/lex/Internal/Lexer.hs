module Internal.Lexer (
    Token(..),
    ParenType(..),
    Lexer
) where

import Data.Text (Text)

data Token =
    Integer Text |
    Binop Text |
    Paren ParenType
    deriving(Eq, Show)

data ParenType = Left | Right deriving(Eq, Show)

type Lexer = Text -> Maybe (Token, Text)
