{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import Prelude hiding (lex)

import qualified Lex as Lex (lex)
import qualified Syn as Syn (syn)

import Utils.Either

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

main :: IO ()
main = do
    content <- Text.IO.readFile "examples/math.t2"
    let tokens = Lex.lex content
        formattedOutput = either formatLexError formatLexTokens tokens
    Text.IO.putStrLn $ formattedOutput
    where
        formatLexTokens = Text.pack . show
        formatLexError lexError = (Text.pack "Lexer encountered unlexable token: ") <> lexError
