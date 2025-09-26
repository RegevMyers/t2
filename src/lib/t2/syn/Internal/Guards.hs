
module Internal.Guards (
    guard,
    guardNotEmpty,
    guardCount,
    guardNoMoreTokens
) where

--import Internal.Ast as Ast (Expression(..), BinopType(..))
import Internal.Parser (Parser(..))

import Lex (Token)

type Guard = Parser ()

guard :: Bool -> Guard
guard b = guardTokens (\_ -> b)

guardNotEmpty :: Guard
guardNotEmpty = guardTokens (not . null)

guardCount :: (Int -> Bool) -> Guard
guardCount p = guardTokens (p . length)

guardNoMoreTokens :: Guard
guardNoMoreTokens = guardTokens null

guardTokens :: ([Token] -> Bool) -> Guard
guardTokens p = Parser $ \tokens -> if p tokens then Just ((), tokens) else Nothing