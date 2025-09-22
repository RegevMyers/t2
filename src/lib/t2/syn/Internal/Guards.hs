
module Internal.Guards (
    guardNotEmpty
) where

--import Internal.Ast as Ast (Expression(..), BinopType(..))
import Internal.Parser (Parser(..))

import Lex (Token)

type Guard = Parser ()

guardNotEmpty :: Guard
guardNotEmpty = guard (not . null)

guard :: ([Token] -> Bool) -> Guard
guard p = Parser $ \tokens ->
    if p tokens then Just ((), tokens) else Nothing
