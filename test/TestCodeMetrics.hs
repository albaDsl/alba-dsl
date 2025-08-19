-- Copyright (c) 2025 albaDsl

module TestCodeMetrics (testCodeMetrics) where

import Alba.Dsl.V1.Bch2025
import Alba.Dsl.V1.Common.StackUntyped (toTyped)
import DslDemo.EllipticCurve.Affine qualified as EA
import DslDemo.EllipticCurve.Jacobian qualified as EJ
import DslDemo.TurtleVm.Bch2025.TurtleVm qualified as T2025
import DslDemo.TurtleVm.Bch2026.TurtleVm qualified as T2026
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testCodeMetrics :: TestTree
testCodeMetrics =
  testGroup
    "Code metrics"
    [ testCase "turtleVm 2025" $
        compileForSize (toTyped (T2025.turtleVm 1 1))
          @?= "1462 opcodes, 1762 bytes.",
      testCase "turtleVm 2026" $
        compileForSize (toTyped (T2026.turtleVm 1))
          @?= "554 opcodes, 1103 bytes.",
      testCase "EC scalar point multiply (Affine)" $
        compileForSize EA.ecMul @?= "26 opcodes, 477 bytes.",
      testCase "EC scalar point multiply (Jacobian)" $
        compileForSize EJ.ecMul @?= "53 opcodes, 603 bytes."
    ]

compileForSize :: forall s s' alt alt'. (S s alt -> S s' alt') -> String
compileForSize prog = sizeStr (compileL2 O1 prog)
