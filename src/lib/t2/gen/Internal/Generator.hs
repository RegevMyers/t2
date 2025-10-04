module Internal.Generator (
    Generator(..)
)
where

import qualified Syn as Ast (Expression)

import Internal.Register (Register)

import Control.Applicative (Alternative(..))
import Data.Text (Text)

data Generator a = Generator { runGenerator :: [Ast.Expression] -> Maybe (a, [Ast.Expression])}

instance Functor (Generator) where
    fmap f (Generator ga) = Generator $ \expressions -> do
        (a, rest) <- ga expressions
        return (f a, rest)

instance Applicative (Generator) where
    pure a = Generator $ \expressions -> Just (a, expressions)
    (Generator gf) <*> (Generator ga) = Generator $ \expressions -> do
        (f, expressions') <- gf expressions
        (a, rest) <- ga expressions'
        return $ (f a, rest)

instance Monad (Generator) where
    (Generator ga) >>= f = Generator $ \expressions -> do
        (a, rest) <- ga expressions
        let (Generator gb) = f a
        gb rest

instance Alternative (Generator) where
    empty = Generator $ \_ -> Nothing
    (Generator ga) <|> (Generator gb) = Generator $ \expressions -> do
        ga expressions <|> gb expressions
