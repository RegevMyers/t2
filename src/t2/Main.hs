module Main (
    main
) where

import qualified Lex as Lex

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

main :: IO ()
main = do
    content <- Text.IO.readFile "ignore/a.txt"
    lex <- Lex.lex content
    Text.IO.putStrLn $ Text.pack $ show lex