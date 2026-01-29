-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2026.Ops where

import Alba.Dsl.V1.Common.CompilerUtils (aop, aops')
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.FunctionState (addCallSite, registerFunction)
import Alba.Dsl.V1.Common.OpcodeL3 (FunctionId (Absolute, Named), OpcodeL3 (..))
import Alba.Dsl.V1.Common.Stack (FN, FNA, S (S), TBool, TBytes, TInt, TNat)
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

opUntil :: FNA s alt (s > TBool) alt -> FNA s alt s alt
opUntil loopBody (S c fs) =
  let (S c' fs') = loopBody (S (aop c OP_BEGIN) fs)
   in S (aop c' OP_UNTIL) fs'

-- Added for completeness. There are other better options to use. AlbaDsl does
-- not offer a way for the user of this function to ensure the idx is not
-- already in use.
opDefine :: FN (s > TBytes > TBytes) s
opDefine (S c fs) = S (aop c OP_DEFINE) fs

-- Define function at an index relative current namespace.
opDefineIdx :: Int -> FN (s > TBytes) s
opDefineIdx idx (S c fs) =
  let fId = Absolute idx
      fs' = fromMaybe err (registerFunction fId fs)
   in S (aops' c [FunctionIndexDef {fId}, Opcode OP_DEFINE]) fs'
  where
    err = error "opDefineIdx: idx already defined."

opDefineNamed :: String -> FN (s > TBytes) s
opDefineNamed name (S c fs) =
  let fId = Named name
      fs' = fromMaybe err (registerFunction fId fs)
   in S (aops' c [FunctionIndexDef {fId}, Opcode OP_DEFINE]) fs'
  where
    err = error "opDefineNamed: name already defined."

-- See opDefine.
opInvoke :: FNA s alt s' alt' -> FNA (s > TBytes) alt s' alt'
opInvoke _prog (S c fs) = S (aop c OP_INVOKE) fs

opInvokeIdx :: Int -> FNA s alt s' alt' -> FNA s alt s' alt'
opInvokeIdx idx _prog (S c fs) =
  let fId = Absolute idx
      fs' = fromMaybe err (addCallSite fId fs)
   in S (aops' c [FunctionIndexRef fId, Opcode OP_INVOKE]) fs'
  where
    err = error (printf "opInvokeIdx: idx not defined: %s" idx)

opInvokeNamed :: String -> FNA s alt s' alt' -> FNA s alt s' alt'
opInvokeNamed name _prog (S c fs) =
  let fId = Named name
      fs' = fromMaybe err (addCallSite fId fs)
   in S (aops' c [FunctionIndexRef fId, Opcode OP_INVOKE]) fs'
  where
    err = error (printf "opInvokeNamed: name not defined: %s" name)

opInvert :: FN (s > TBytes) (s > TBytes)
opInvert (S c fs) = S (aop c OP_INVERT) fs

opLShiftNum :: FN (s > TInt > TNat) (s > TInt)
opLShiftNum (S c fs) = S (aop c OP_LSHIFTNUM) fs

opRShiftNum :: FN (s > TInt > TNat) (s > TInt)
opRShiftNum (S c fs) = S (aop c OP_RSHIFTNUM) fs

opLShiftBin :: FN (s > TBytes > TNat) (s > TBytes)
opLShiftBin (S c fs) = S (aop c OP_LSHIFTBIN) fs

opRShiftBin :: FN (s > TBytes > TNat) (s > TBytes)
opRShiftBin (S c fs) = S (aop c OP_RSHIFTBIN) fs
