{-# LANGUAGE OverloadedStrings #-}

module Syn
--(
--    syn
--)
where

import Internal.Ast as Ast (Expression(..))

import Lex (Token(..))

import Internal.Parsers
import Internal.Parser

syn :: [Token] -> Expression
syn = undefined
