module Gen (
    gen
) where

import qualified Syn as Ast (Expression)

import qualified Internal.X86_64.Generator as X86_64 (generate)

import Data.Text (Text)

data Arch =
    X86_64
    deriving(Eq)

gen :: Arch -> [Ast.Expression] -> Maybe Text
gen arch expressions = do
    generator <- lookup arch genTable
    generator expressions

genTable :: [(Arch, [Ast.Expression] -> Maybe Text)]
genTable = [
        (X86_64, X86_64.generate)
    ]