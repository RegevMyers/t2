module Lex (
    lex,
    Token(..),
) where

import Internal.Lexer (Token(..))
import Internal.Lexers (lexTokens)

import Data.Text (Text)

import Prelude hiding (lex)

lex :: Text -> Either Text [Token]
lex text = sequenceA $ lexTokens text
