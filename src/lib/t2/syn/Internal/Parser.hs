module Internal.Parser (
    Parser(..)
) where

import qualified Lex (Token)

import Control.Applicative (Alternative(..))

data Parser a = Parser { runParser :: [Lex.Token] -> Maybe (a, [Lex.Token])}

instance Functor (Parser) where
    fmap f (Parser pa) = Parser $ \tokens -> do
        (a, rest) <- pa tokens
        return (f a, rest)

instance Applicative (Parser) where
    pure a = Parser $ \tokens -> Just (a, tokens)
    (Parser pf) <*> (Parser pa) = Parser $ \tokens -> do
        (f, tokens') <- pf tokens
        (a, rest)    <- pa tokens'
        return (f a, rest)

instance Monad (Parser) where
    (Parser pa) >>= f = Parser $ \tokens -> do
        (a, rest) <- pa tokens
        let (Parser pb) = f a
        pb rest

instance Alternative (Parser) where
    empty = Parser $ \_ -> Nothing
    (Parser pa) <|> (Parser pb) = Parser $ \tokens ->
        pa tokens <|> pb tokens
