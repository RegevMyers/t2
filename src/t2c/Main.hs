module Main (
    main
) where

import Prelude hiding (lex)

import qualified Lex as Lex (lex)
import qualified Syn as Syn (syn)
import qualified Syn as Ast (Expression)

import Data.Text
import qualified Data.Text.IO as Text.IO
import Control.Monad ((>=>))
import System.Environment as Env (getArgs)

main :: IO ()
main = do
    args <- Env.getArgs
    content <- Text.IO.readFile $ args !! 0
    let ast = compile content
    Text.IO.putStrLn $ case ast of Right ast' -> (pack . show) ast'

compile :: Text -> Either Text Ast.Expression
compile = Lex.lex >=> Syn.syn