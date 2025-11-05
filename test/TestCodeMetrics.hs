-- Copyright (c) 2025 albaDsl

module TestCodeMetrics (testCodeMetrics) where

import Alba.Dsl.V1.Bch2026
import Alba.Dsl.V1.Common.StackUntyped (toTyped)
import Alba.Vm.Bch2026 (VmMetrics (..))
import DslDemo.EllipticCurve.Affine qualified as EA
import DslDemo.EllipticCurve.Constants (g)
import DslDemo.EllipticCurve.Jacobian qualified as EJ
import DslDemo.EllipticCurve.JacobianWindowed qualified as EJW
import DslDemo.EllipticCurve.Point (isEqual, pushPoint)
import DslDemo.TurtleVm.Bch2025.TurtleVm qualified as T2025
import DslDemo.TurtleVm.Bch2026.TurtleVm qualified as T2026
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestUtils (TestResult (..))
import TestUtils2026 (evaluateProg)

testCodeMetrics :: TestTree
testCodeMetrics =
  testGroup
    "Code metrics"
    [ testGroup
        "Code size"
        [ testCase "turtleVm 2025" $
            compileForSize (toTyped (T2025.turtleVm 1 1))
              @?= "1462 opcodes, 1762 bytes.",
          testCase "turtleVm 2026" $
            compileForSize (toTyped (T2026.turtleVm 1))
              @?= "554 opcodes, 1103 bytes.",
          testCase "EC scalar point multiply (Affine)" $
            compileForSize EA.ecMul @?= "38 opcodes, 452 bytes.",
          testCase "EC scalar point multiply (Jacobian)" $
            compileForSize EJ.ecMul @?= "62 opcodes, 630 bytes.",
          testCase "EC scalar point multiply (Windowed Jacobian)" $
            compileForSize EJW.ecMul @?= "56 opcodes, 626 bytes.",
          testCase "EC scalar point multiply (Windowed Jacobian / tbl setup)" $
            compileForSize (EJW.setupTable # EJW.ecMul)
              @?= "72 opcodes, 723 bytes."
        ],
      testGroup
        "Cost"
        [ testCase "EC scalar point multiply (Windowed Jacobian / tbl setup)" $
            costOfWindowedMul @?= 33_532_367
        ]
    ]

compileForSize :: forall s s' alt alt'. (S s alt -> S s' alt') -> String
compileForSize prog = sizeStr (fst $ compileL2 O1 prog)

costOfWindowedMul :: Int
costOfWindowedMul =
  case evaluateProg prog of
    Right tr -> tr.metrics.cost
    Left _ -> error ""
  where
    gTable = nat 100
    prog =
      begin
        # (gTable # g # EJW.setupTable)
        # gTable
        # nat 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364140
        # EJW.ecMul
        # pushPoint
          0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
          0xB7C52588D95C3B9AA25B0403F1EEF75702E84BB7597AABE663B82F6F04EF2777
        # (isEqual # opVerify)
