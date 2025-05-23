-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Bch2025.Contract.Prelude where

import Alba.Dsl.V1.Bch2025.Lang (bytes', nat)
import Alba.Dsl.V1.Bch2025.Ops
  ( op0,
    opCheckSig,
    opDup,
    opEqual,
    opEqualVerify,
    opGreaterThanOrEqual,
    opHash160,
    opIf,
    opSubUnsafe,
    opVerify,
  )
import Alba.Dsl.V1.Bch2025.Stack (StackNum, THash160)
import Alba.Dsl.V1.Common.FlippedCons (type (>))
import Alba.Dsl.V1.Common.Lang (begin, (#))
import Alba.Dsl.V1.Common.Stack (FN, FNA, TBool, TNat, TPubKey, TSig)
import Alba.Vm.Common.BasicTypes (Bytes)

p2shScriptPubKey :: Bytes -> FN (s > THash160) (s > TBool)
p2shScriptPubKey scriptHash =
  begin
    # opHash160
    # bytes' scriptHash
    # opEqual

p2pkhScriptPubKey :: Bytes -> FN (s > TSig > TPubKey) (s > TBool)
p2pkhScriptPubKey pubKeyHash =
  begin
    # opDup
    # opHash160
    # bytes' pubKeyHash
    # opEqualVerify
    # opCheckSig

natSub :: FN (s > TNat > TNat) (s > TNat)
natSub =
  begin
    # opSubUnsafe
    # opDup
    # (nat 0 # opGreaterThanOrEqual # opVerify)

ifZero ::
  (StackNum x1) =>
  FNA s alt s' alt' ->
  FNA s alt s' alt' ->
  FNA (s > x1) alt s' alt'
ifZero ifOps elseOps = op0 # opEqual # opIf ifOps elseOps
