{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lex (
    lex
) where

import Internal.Lexer

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Char (isDigit)
import Control.Applicative ((<|>))

import Prelude hiding (lex)

lex :: Text -> Either Text [Token]
lex text = sequenceA $ lexWords text

lexWords :: Text -> [Either Text Token]
lexWords "" = []
lexWords text =
    case lexWord text of
        Just (token, rest) -> Right token : (lexWords . Text.strip) rest
        Nothing            -> [Left text]

lexWord :: Lexer
lexWord text =
    lexBinop text <|>
    lexInteger text

lexBinop :: Lexer
lexBinop text
    | elem firstChar [ "+", "-", "*", "/" ] = Just ((Binop, firstChar), rest)
    | otherwise                             = Nothing
    where
        (firstChar, rest) = Text.splitAt 1 text

lexInteger :: Lexer
lexInteger text
    | (not . Text.null) integer = Just ((Integer, integer), rest)
    | otherwise                 = Nothing
    where
        (integer, rest) = Text.span isDigit text
