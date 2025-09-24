
module Internal.Guards (
    assert,
    guardNotEmpty,
    guardCount
) where

--import Internal.Ast as Ast (Expression(..), BinopType(..))
import Internal.Parser (Parser(..))

import Lex (Token)

type Guard = Parser ()

assert :: Bool -> Guard
assert b = guard (\_ -> b)

guardNotEmpty :: Guard
guardNotEmpty = guard (not . null)

guardCount :: (Int -> Bool) -> Guard
guardCount p = guard (p . length)

guard :: ([Token] -> Bool) -> Guard
guard p = Parser $ \tokens -> if p tokens then Just ((), tokens) else Nothing