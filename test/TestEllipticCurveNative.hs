-- Copyright (c) 2026 albaDsl

module TestEllipticCurveNative (testEllipticCurveNative) where

import DslDemo.EllipticCurve.Constants qualified as C
import DslDemo.EllipticCurve.Native.Jacobian qualified as NJ
import DslDemo.EllipticCurve.Native.JacobianPlain qualified as NJ
import DslDemo.EllipticCurve.Native.JacobianWNafGlv qualified as NJWNG
import QuickCheckSupport (Bits256 (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, testProperty, (==>))
import Prelude hiding (drop, iterate)

testEllipticCurveNative :: TestTree
testEllipticCurveNative =
  testGroup
    "Elliptic Curve (Native)"
    [ testProperty "Scalar multiply additivity (wNAF & GLV)" propAdditivity,
      testProperty
        "Scalar multiply comparison (wNAF & GLV / plain Jacobian)"
        propComparison
    ]

propAdditivity :: Bits256 -> Bits256 -> Property
propAdditivity (Bits256 a) (Bits256 b) =
  (a > 0 && a < C.n && b > 0 && b < C.n) ==>
    let a' = fromIntegral a
        b' = fromIntegral b
        p =
          NJ.fromJacobian $
            NJ.ecAdd (NJWNG.ecMul a' NJ.g) (NJWNG.ecMul b' NJ.g)
        q = NJ.fromJacobian $ NJWNG.ecMul (a' + b') NJ.g
     in p == q

propComparison :: Bits256 -> Property
propComparison (Bits256 n) =
  (n > 0 && n < C.n) ==>
    let n' = fromIntegral n
        p = NJ.fromJacobian $ (NJWNG.ecMul n' NJ.g)
        q = NJ.fromJacobian $ (NJ.ecMul n' NJ.g)
     in p == q
