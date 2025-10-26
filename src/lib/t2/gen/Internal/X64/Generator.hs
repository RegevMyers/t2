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
    binopText <- genBinop binop exprAResult exprBResult
    freeScratchRegister exprBResult
    return (mconcat [exprAText, exprBText, binopText], exprAResult)
genBinexpr _ = lift Nothing

genBinop :: Ast.Binop -> Register -> Register -> Generator Text
genBinop (Ast.Binop binopType _ _) = do
    lookup binopType binopTable ?? pure "a"
    where
        binopTable = [
                (Ast.Plus,   genAdd),
                (Ast.Minus,  genSub),
                (Ast.Times,  genMul),
                (Ast.Divide, genDiv)
            ] --TODO support Exp

genAdd :: Register -> Register -> Generator Text
genAdd regA regB = do
    freeScratchRegister regB
    return (Fmt.instruction "add" [(pack . show) regA, (pack . show) regB])

genSub :: Register -> Register -> Generator Text
genSub regA regB = do
    freeScratchRegister regB
    return (Fmt.instruction "sub" [(pack . show) regA, (pack . show) regB])

genMul :: Register -> Register -> Generator Text
genMul regA regB = do
    freeScratchRegister regA
    freeScratchRegister regB
    return (
            Fmt.instruction "mov" ["rax", (pack . show) regA] <>
            Fmt.instruction "mul" [(pack . show) regB] <>
            Fmt.instruction "mov" [(pack . show) regA, "rax"]
        ) -- TODO: return RDX too?

genDiv :: Register -> Register -> Generator Text
genDiv regA regB = do
    freeScratchRegister regA
    freeScratchRegister regB
    return (
            Fmt.instruction "mov" ["rax", (pack . show) regA] <>
            Fmt.instruction "div" [(pack . show) regB] <>
            Fmt.instruction "mov" [(pack . show) regA, "rax"]
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
