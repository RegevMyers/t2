{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lex (
    lex,
    lexTokens,
    lexToken
) where

import Internal.Lexer

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Char (isDigit)
import Control.Applicative ((<|>))

import Prelude hiding (lex)

lex :: Text -> Either Text [Token]
lex text = sequenceA $ lexTokens text

lexTokens :: Text -> [Either Text Token]
lexTokens "" = []
lexTokens text =
    case lexToken text of
        Just (token, rest) -> Right token : (lexTokens . Text.stripStart) rest
        Nothing            -> [Left text]

lexToken :: Lexer
lexToken text =
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
