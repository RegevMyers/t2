module Main (
    main
) where

import qualified Data.Text.IO as Text.IO

main :: IO ()
main = do
    content <- Text.IO.readFile "ignore/a.txt"
    Text.IO.putStrLn content