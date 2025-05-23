-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.VmOpArithmetic (evalOpArithmetic) where

import Alba.Vm.Bch2025.Utils qualified as VU
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (StackElement)
import Alba.Vm.Common.VmState (VmState (..))

{- ORMOLU_DISABLE -}
import Alba.Vm.Bch2025.Utils
  ( bor1, bor2, bor3, nc1, nc2, nc3, op1,
    op1a, op2, op2a, op2q, op2v, op3, verifyEqual,
  )

evalOpArithmetic :: OpcodeL2 -> VmState -> Maybe (Either ScriptError VmState)
evalOpArithmetic op st@VmState {params} =
  case op of
    OP_1ADD ->    op1a st ((ir1 . ia1 . nc1) succ)
    OP_1SUB ->    op1a st ((ir1 . ia1 . nc1) pred)
    OP_NEGATE ->  op1a st ((ir1 . ia1 . nc1) negate)
    OP_ABS ->     op1a st ((ir1 . ia1 . nc1) abs)
    OP_ADD ->     op2a st ((ir2 . ia2 . nc2) (+))
    OP_SUB ->     op2a st ((ir2 . ia2 . nc2) (-))
    OP_MUL ->     op2q st ((ir2 . ia2 . nc2) (*))
    OP_DIV ->     op2q st ((ir2 . ia2 . zc2 SeDivideByZero) quot)
    OP_MOD ->     op2q st ((ir2 . ia2 . zc2 SeModByZero) rem)
    OP_MIN ->     op2a st ((ir2 . ia2 . nc2) min)
    OP_MAX ->     op2a st ((ir2 . ia2 . nc2) max)

    OP_BOOLAND -> op2 st ((bor2 . boa2 . nc2) (&&))
    OP_BOOLOR ->  op2 st ((bor2 . boa2 . nc2) (||))
    OP_NOT ->     op1 st ((bor1 . boa1 . nc1) not)

    OP_0NOTEQUAL ->          op1  st ((bor1 . ia1 . nc1) (/= 0))
    OP_NUMEQUAL ->           op2  st ((bor2 . ia2 . nc2) (==))
    OP_NUMEQUALVERIFY ->     op2v st (ia2 (verifyEqual SeNumEqualVerify))
    OP_NUMNOTEQUAL ->        op2  st ((bor2 . ia2 . nc2) (/=))
    OP_LESSTHAN ->           op2  st ((bor2 . ia2 . nc2) (<))
    OP_GREATERTHAN ->        op2  st ((bor2 . ia2 . nc2) (>))
    OP_LESSTHANOREQUAL ->    op2  st ((bor2 . ia2 . nc2) (<=))
    OP_GREATERTHANOREQUAL -> op2  st ((bor2 . ia2 . nc2) (>=))
    OP_WITHIN ->             op3  st ((bor3 . ia3 . nc3) inRange)

    _ -> Nothing
  where
    ia1 :: (Integer -> Either ScriptError a) ->
           (StackElement -> Either ScriptError a)
    ia1 = VU.ia1 params

    ia2 :: (Integer -> Integer -> Either ScriptError a) ->
           (StackElement -> StackElement -> Either ScriptError a)
    ia2 = VU.ia2 params

    ia3 = VU.ia3 params

    boa1 = VU.boa1 params
    boa2 = VU.boa2 params

    ir1 = VU.ir1 params
    ir2 = VU.ir2 params

    -- ZeroCheck for divisor.
    zc2 err _f _x 0 = Left err
    zc2 _err f x y = Right $ f x y

    inRange x1 x2 x3 = x2 <= x1 && x1 < x3
{- ORMOLU_ENABLE -}
