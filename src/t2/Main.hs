{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import qualified Lex as Lex

import Utils.Maybe

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

main :: IO ()
main = do
    content <- Text.IO.readFile "examples/math.t2"
    let lex' = Lex.lex content ?? []
    Text.IO.putStrLn $ Text.pack $ show lex'