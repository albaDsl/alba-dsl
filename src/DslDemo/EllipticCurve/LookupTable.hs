-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.LookupTable
  ( defineConstant,
    getConstant,
    toPushOp,
  )
where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Common.RuntimeLib (toPushOp)
import Prelude (undefined)

defineConstant :: Fn (s > TBytes > TNat) s
defineConstant = fn (n2b . opSwap . toPushOp . opSwap . opDefine)

n2b :: Fn (s > TNat) (s > TBytes)
n2b = cast

getConstant :: Fn (s > TNat) (s > TBytes)
getConstant = n2b . opInvoke get
  where
    get :: Fn s (s > TBytes)
    get = undefined
