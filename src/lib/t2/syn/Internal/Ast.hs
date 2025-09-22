module Internal.Ast (
    Expression(..),
    BinopType(..)
) where

data Expression =
    Binop Expression BinopType Expression |
    Integer Integer
    deriving(Show)

data BinopType =
    Plus |
    Minus |
    Times |
    Divide
    deriving(Show)