module Internal.Ast (
    Expression(..),
    BinopType(..),
    BinopPower,
    BinopAssociativity(..),
    Binop(..)
) where

data Expression =
    Binexpr Binop Expression Expression |
    Integer Integer
    deriving(Show)

data BinopType = Plus | Minus | Times | Divide | Exp deriving(Show)
type BinopPower = Int
data BinopAssociativity = LeftAssociative | RightAssociative | Unassociative deriving(Eq, Show)
data Binop = Binop BinopType BinopPower BinopAssociativity deriving(Show)
