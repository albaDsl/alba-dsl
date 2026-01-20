-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.LookupTable
  ( defineConstant,
    getConstant,
    toPushOp,
  )
where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Common.RuntimeLib (toPushOp)

defineConstant :: FN (s > TBytes > TNat) s
defineConstant = function (n2b # opSwap # toPushOp # opSwap # opDefine)

n2b :: FN (s > TNat) (s > TBytes)
n2b = cast

getConstant :: FN (s > TNat) (s > TBytes)
getConstant = n2b # opInvoke get
  where
    get :: FN s (s > TBytes)
    get = undefined
