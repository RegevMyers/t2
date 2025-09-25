module Lex (
    lex,
    Token(..),
    ParenType(..)
) where

import Internal.Lexer (Token(..), ParenType(..))
import Internal.Lexers (lexTokens)

import Data.Text (Text)

import Prelude hiding (lex)

lex :: Text -> Either Text [Token]
lex text = sequenceA $ lexTokens text
