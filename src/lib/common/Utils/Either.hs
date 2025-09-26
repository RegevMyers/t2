module Utils.Either (
    (??)
) where

infixl 1 ??
(??) :: Either a b -> b -> b
e ?? r = case e of
    Right r' -> r'
    Left _ -> r
