-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.LookupTable
  ( defineConstant,
    getConstant,
    toPushOp,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack ((:>)),
    TBytes,
    TFunctionId,
    fn,
    opDefine,
    opInvoke,
    opSwap,
    (.),
  )
import Alba.Dsl.V1.Common.RuntimeLib (toPushOp)
import Prelude (undefined)

defineConstant :: Fn (s :> TBytes :> TFunctionId) s
defineConstant = fn (opSwap . toPushOp . opSwap . opDefine)

getConstant :: Fn (s :> TFunctionId) (s :> TBytes)
getConstant = opInvoke get
  where
    get :: Fn s (s :> TBytes)
    get = undefined
