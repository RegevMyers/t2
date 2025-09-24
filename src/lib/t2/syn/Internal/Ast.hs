module Internal.Ast (
    Expression(..),
    BinopType(..),
    BinopAssociativity(..),
    Binop(..)
) where

data Expression =
    Binexpr Binop Expression Expression |
    Integer Integer
    deriving(Show)

data BinopType = Plus | Minus | Times | Divide deriving(Show)
type BinopPower = Int
data BinopAssociativity = LeftAssociative | RightAssociative | Unassociative deriving(Show)
data Binop = Binop BinopType BinopPower BinopAssociativity deriving(Show)
