module Lex (
    lex
) where

import Prelude hiding (lex)

import Data.Text (Text)

lex :: Text -> IO [Int]
lex t = pure [1,2,3]