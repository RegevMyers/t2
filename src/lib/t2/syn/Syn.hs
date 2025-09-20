{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Syn (
    syn,
    TokenType(..)
) where

import Lex (lex)

import Prelude hiding (lex)

syn :: [Token] -> Ast
