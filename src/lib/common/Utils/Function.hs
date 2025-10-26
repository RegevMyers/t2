module Utils.Function where

pipe :: [a -> a] -> a -> a
pipe functions value = foldl (\v f -> f v) value functions

infixr 1 <<|
(<<|) :: [a -> a] -> a -> a
functions <<| value = pipe functions value

infixl 1 |>>
(|>>) :: a -> [a -> a] -> a
(|>>) = flip (<<|)

infixl 4 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)