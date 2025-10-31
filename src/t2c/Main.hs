module Main (
    main
) where

import Prelude hiding (lex)

import qualified Lex as Lex (lex)
import qualified Syn as Syn (syn)
import qualified Syn as Ast (Expression)
import qualified Gen as Gen (gen, Arch(..))

import Data.Text
import qualified Data.Text.IO as Text.IO
import Control.Monad ((>=>))
import System.Environment as Env (getArgs)

main :: IO ()
main = do
    args <- Env.getArgs
    content <- Text.IO.readFile $ args !! 0
    let text = compile content
    Text.IO.putStrLn $ case text of Right text' -> text'

compile :: Text -> Either Text Text
compile = Lex.lex >=> Syn.syn >=> Gen.gen Gen.X64