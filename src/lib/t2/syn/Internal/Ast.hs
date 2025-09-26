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
    deriving(Eq, Show)

data BinopType = Plus | Minus | Times | Divide | Exp deriving(Eq, Show)
type BinopPower = Int
data BinopAssociativity = LeftAssociative | RightAssociative | Unassociative deriving(Eq, Show)
data Binop = Binop BinopType BinopPower BinopAssociativity deriving(Eq, Show)
