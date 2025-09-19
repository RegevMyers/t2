{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lex (
    lex
) where

import Internal.Lexer

import Data.Text (Text, all, splitOn)

import Prelude hiding (lex, all)

lex :: Text -> Maybe [Token]
lex text =
    mapM lexWord $ splitOn " " text

lexWord :: Text -> Maybe Token
lexWord word =
    (, word) <$> classification
    where
        classification = classify word

classify :: Text -> Maybe TokenType
classify text
    | elem text [ "+", "-", "*", "/" ]  = Just Binop
    | all (`elem` [ '0' .. '9' ]) text  = Just Integer
    | otherwise                         = Nothing
