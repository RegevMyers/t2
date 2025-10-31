module Gen (
    gen,
    Arch(..)
) where

import qualified Syn as Ast (Expression)

import qualified Internal.X64.Generator as X64 (generate)

import Data.Text (Text)

data Arch =
    X64
    deriving(Eq)

gen :: Arch -> [Ast.Expression] -> Either Text Text
gen arch expressions =
    case do
        generator <- lookup arch genTable
        generator expressions
    of
        Just text -> Right text
        Nothing   -> Left "i cooka da pizza"

genTable :: [(Arch, [Ast.Expression] -> Maybe Text)]
genTable = [
        (X64, X64.generate)
    ]