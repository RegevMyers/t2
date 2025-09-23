module Internal.Ast (
    Expression(..),
    BinopType(..)
) where

data Expression =
    Binop BinopType Expression Expression |
    Integer Integer
    deriving(Show)

data BinopType =
    Plus |
    Minus |
    Times |
    Divide
    deriving(Show)