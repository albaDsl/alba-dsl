-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.Field
  ( TFe,
    mkFe,
    pushFe,
    feAdd,
    feSub,
    feNeg,
    feMul,
    feSquare,
    feCube,
    feQuartic,
    feInv,
    primeModulus,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Stack (..),
    StackEntry,
    TInt,
    TNat,
    cast,
    castStack,
    constant,
    fn,
    i2nUnsafe,
    int,
    n2i,
    nat,
    op2,
    opVerify,
    opWithin,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.BlobEqClass (BlobEq (..))
import Alba.Dsl.V1.Bch2026.Contract.BlobEqUtils
  ( blobEqEqual,
    blobEqEqualVerify,
    blobEqRecord,
  )
import Alba.Dsl.V1.Bch2026.Contract.Math (pow')
import Alba.Dsl.V1.Bch2026.Contract.Prelude (add, dup, mod, mul, sub, swap)
import Control.Exception (assert)
import DslDemo.EllipticCurve.Constants (p)
import Numeric.Natural (Natural)
import Prelude (fromIntegral, (&&), (-), (<=), (>=))

-- Field element type. Represents the secp256k1 Fp field.
data TFe

instance StackEntry TFe

instance BlobEq TFe where
  equal = blobEqEqual
  equalVerify = blobEqEqualVerify
  blobEqRec = blobEqRecord

mkFe :: Fn (s :> TNat) (s :> TFe)
mkFe =
  dup . nat 0 . nat (fromIntegral p - 1) . opWithin . opVerify . n2i . fromRaw

pushFe :: Natural -> Fn s (s :> TFe)
pushFe x =
  assert (x >= 0 && x <= (fromIntegral p - 1)) (int (fromIntegral x) . fromRaw)

feAdd :: Fn (s :> TFe :> TFe) (s :> TFe)
feAdd = fn (toRaw2 . add . primeModulus . mod . fromRaw)

feSub :: Fn (s :> TFe :> TFe) (s :> TFe)
feSub = fn (toRaw2 . sub . primeModulus . add . primeModulus . mod . fromRaw)

feNeg :: Fn (s :> TFe) (s :> TFe)
feNeg = fn (int 0 . fromRaw . swap . feSub)

feMul :: Fn (s :> TFe :> TFe) (s :> TFe)
feMul = fn (toRaw2 . mul . primeModulus . mod . fromRaw)

feSquare :: Fn (s :> TFe) (s :> TFe)
feSquare = dup . feMul

feCube :: Fn (s :> TFe) (s :> TFe)
feCube = fn (dup . feSquare . feMul)

feQuartic :: Fn (s :> TFe) (s :> TFe)
feQuartic = feSquare . feSquare

feInv :: Fn (s :> TFe) (s :> TFe)
feInv = fn (toRaw . primeModulusMinus2 . i2nUnsafe . pow' feMul' . fromRaw)
  where
    primeModulusMinus2 :: Fn s (s :> TInt)
    primeModulusMinus2 = primeModulus . op2 . sub

    feMul' :: Fn (s :> TInt :> TInt) (s :> TInt)
    feMul' = fromRaw2 . feMul . toRaw

primeModulus :: Fn s (s :> TInt)
primeModulus = constant (int (fromIntegral p))

fromRaw :: Fn (s :> TInt) (s :> TFe)
fromRaw = cast

fromRaw2 :: Fn (s :> TInt :> TInt) (s :> TFe :> TFe)
fromRaw2 = castStack

toRaw :: Fn (s :> TFe) (s :> TInt)
toRaw = cast

toRaw2 :: Fn (s :> TFe :> TFe) (s :> TInt :> TInt)
toRaw2 = castStack
