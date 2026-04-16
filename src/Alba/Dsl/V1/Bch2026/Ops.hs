-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Ops where

import Alba.Dsl.V1.Bch2026.Stack (TCode, TFunctionId)
import Alba.Dsl.V1.Common.CompilerUtils (aop, aops')
import Alba.Dsl.V1.Common.FunctionState (addCallSite, registerFunction)
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId (Absolute, Named), OpcodeL3 (..))
import Alba.Dsl.V1.Common.Stack
  ( Fn,
    FnA,
    S (..),
    Stack (..),
    TBool,
    TBytes,
    TInt,
    TNat,
  )
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Control.Arrow ((>>>))
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

opUntil :: FnA s alt (s :> TBool) alt -> FnA s alt s alt
opUntil loopBody = aop OP_BEGIN >>> loopBody >>> aop OP_UNTIL

-- Added for completeness. There are other better options to use. AlbaDsl does
-- not offer a way for the user of this function to ensure the idx is not
-- already in use.
opDefine :: Fn (s :> TCode :> TFunctionId) s
opDefine = aop OP_DEFINE

-- Define function at an index relative current namespace.
opDefineIdx :: Int -> Fn (s :> TCode) s
opDefineIdx idx st =
  let fId = Absolute idx
      fs = fromMaybe err (registerFunction fId st.fs)
   in aops' [FunctionIndexDef {fId}, Opcode OP_DEFINE] (st {fs = fs})
  where
    err = error "opDefineIdx: idx already defined."

opDefineNamed :: String -> Fn (s :> TCode) s
opDefineNamed name st =
  let fId = Named name
      fs = fromMaybe err (registerFunction fId st.fs)
   in aops' [FunctionIndexDef {fId}, Opcode OP_DEFINE] (st {fs = fs})
  where
    err = error "opDefineNamed: name already defined."

-- See opDefine.
opInvoke :: FnA s alt s' alt' -> FnA (s :> TFunctionId) alt s' alt'
opInvoke _prog = aop OP_INVOKE

opInvokeIdx :: Int -> FnA s alt s' alt' -> FnA s alt s' alt'
opInvokeIdx idx _prog st =
  let fId = Absolute idx
      fs = fromMaybe err (addCallSite fId st.fs)
   in aops' [FunctionIndexRef fId, Opcode OP_INVOKE] (st {fs = fs})
  where
    err = error (printf "opInvokeIdx: idx not defined: %s" idx)

opInvokeNamed :: String -> FnA s alt s' alt' -> FnA s alt s' alt'
opInvokeNamed name _prog st =
  let fId = Named name
      fs = fromMaybe err (addCallSite fId st.fs)
   in aops' [FunctionIndexRef fId, Opcode OP_INVOKE] (st {fs = fs})
  where
    err = error (printf "opInvokeNamed: name not defined: %s" name)

opInvert :: Fn (s :> TBytes) (s :> TBytes)
opInvert = aop OP_INVERT

opLShiftNum :: Fn (s :> TInt :> TNat) (s :> TInt)
opLShiftNum = aop OP_LSHIFTNUM

opRShiftNum :: Fn (s :> TInt :> TNat) (s :> TInt)
opRShiftNum = aop OP_RSHIFTNUM

opLShiftBin :: Fn (s :> TBytes :> TNat) (s :> TBytes)
opLShiftBin = aop OP_LSHIFTBIN

opRShiftBin :: Fn (s :> TBytes :> TNat) (s :> TBytes)
opRShiftBin = aop OP_RSHIFTBIN
