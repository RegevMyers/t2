module Internal.X86_64.Generator (
    generate
) where

import Syn as Ast (Expression(..))

import Internal.Generator
import Internal.X86_64.Registers

import Control.Monad.State as State (State, get, put, modify, StateT, lift, runStateT)
import qualified Data.Map as Map (insert, toList)
import Data.Text (Text, pack)
import Data.List (find)

type Generator = StateT Scratchpad Maybe

generate :: [Ast.Expression] -> Maybe Text
generate (expression:expressions) = runStateT scratchRegisters (genInteger expression)

genInteger :: Ast.Expression -> Generator (Text, Register)
genInteger (Ast.Integer integer) = do
    register <- getScratchRegister
    let text = pack $ "    MOV     " ++ show register ++ ", " ++ show integer
    return (text, register)
genInteger _ = lift Nothing

getScratchRegister :: Generator Register
getScratchRegister = do
    scratchpad <- get
    register <- lift $ findFreeRegister scratchpad
    let newState = Map.insert register True scratchpad
    put newStatef
    return register
    where
        findFreeRegister scratchpad = fst <$> (find (not . snd) $ Map.toList scratchpad)

freeScratchRegister :: Register -> Generator ()
freeScratchRegister register = modify (Map.insert register False)
