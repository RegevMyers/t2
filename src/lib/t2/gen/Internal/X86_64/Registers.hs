module Internal.X86_64.Registers (
    Register(..),
    Scratchpad
) where

import Internal.Generator

import Data.Map as Map (Map, fromList)

import Data.Data (Data, toConstr)
import Data.Char (toLower)
import Data.List (map)

data Register =
    RAX | RBX | RCX | RDX |
    RDI | RSI | RBP | RSP |
    R8  | R9  | R10 | R11 |
    R12 | R13 | R14 | R15
    deriving(Eq, Ord, Enum, Data)

instance Show Register where
  show = map toLower . show . toConstr

type Scratchpad = Map Register Bool

scratchRegisters :: Scratchpad
scratchRegisters = Map.fromList [(r, False) | r <- [ R8 .. R15 ]]
