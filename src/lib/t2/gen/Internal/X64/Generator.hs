module Internal.X64.Generator (
    generate
) where

import Syn as Ast (Expression(..), Binop(..), BinopType(..))

import Internal.X64.Registers (Register, Scratchpad, scratchRegisters)
import qualified Internal.X64.Format as Fmt

import Control.Monad.State as State (State, get, put, modify, StateT, lift, evalStateT)
import qualified Data.Map as Map (insert, toList)
import Data.Text (Text, pack)
import Data.List (find)
import Utils.Function ((<&>))
import Control.Applicative (Alternative((<|>)))
import Control.Monad ((>=>))
import Utils.Maybe ((??))

type Generator = StateT Scratchpad Maybe
type ExpressionGenerator = Ast.Expression -> Generator (Text, Register)

generate :: [Ast.Expression] -> Maybe Text
generate [] = Just ""
generate (expression:_) = evalStateT (genExpression expression) scratchRegisters <&> fst

genExpression :: ExpressionGenerator
genExpression e = genBinexpr e <|> genInteger e

genBinexpr :: ExpressionGenerator
genBinexpr (Ast.Binexpr binop exprA exprB) = do
    (exprAText, exprAResult) <- genExpression exprA
    (exprBText, exprBResult) <- genExpression exprB
    (binopText, binopResult) <- genBinop binop exprAResult exprBResult
    if binopResult /= exprAResult then freeScratchRegister exprAResult else pure ()
    if binopResult /= exprBResult then freeScratchRegister exprBResult else pure ()
    return (mconcat [exprAText, exprBText, binopText], binopResult)
genBinexpr _ = lift Nothing

genBinop :: Ast.Binop -> Register -> Register -> Generator (Text, Register)
genBinop (Ast.Binop binopType _ _) regA regB = do
    binop <- lift $ lookup binopType binopTable
    binop regA regB
    where
        binopTable = [
                (Ast.Plus,   genAdd),
                (Ast.Minus,  genSub),
                (Ast.Times,  genMul),
                (Ast.Divide, genDiv)
            ] --TODO support Exp

genAdd :: Register -> Register -> Generator (Text, Register)
genAdd regA regB = do
    return (Fmt.instruction "add" [(pack . show) regA, (pack . show) regB], regA)

genSub :: Register -> Register -> Generator (Text, Register)
genSub regA regB = do
    return (Fmt.instruction "sub" [(pack . show) regA, (pack . show) regB], regA)

genMul :: Register -> Register -> Generator (Text, Register)
genMul regA regB = do
    return (
            Fmt.instruction "mov" ["rax", (pack . show) regA] <>
            Fmt.instruction "mul" [(pack . show) regB] <>
            Fmt.instruction "mov" [(pack . show) regA, "rax"],
            regA
        ) -- TODO: return RDX too?

genDiv :: Register -> Register -> Generator (Text, Register)
genDiv regA regB = do
    return (
            Fmt.instruction "mov" ["rax", (pack . show) regA] <>
            Fmt.instruction "div" [(pack . show) regB] <>
            Fmt.instruction "mov" [(pack . show) regA, "rax"],
            regA
        ) -- TODO: return RDX too?

--genExp ??

genInteger :: ExpressionGenerator
genInteger (Ast.Integer integer) = do
    register <- getScratchRegister
    let text = Fmt.instruction "mov" [(pack . show) register, (pack . show) integer]
    return (text, register)
genInteger _ = lift Nothing

getScratchRegister :: Generator Register
getScratchRegister = do
    scratchpad <- get
    register <- lift $ findFreeRegister scratchpad
    let newState = Map.insert register True scratchpad
    put newState
    return register
    where
        findFreeRegister scratchpad = fst <$> (find (not . snd) $ Map.toList scratchpad)

freeScratchRegister :: Register -> Generator ()
freeScratchRegister register = modify (Map.insert register False)
