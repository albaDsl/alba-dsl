-- Copyright (c) 2025 albaDsl

import Test.Tasty (TestTree, defaultMain, testGroup)
import TestArguments (testArguments)
import TestBitwise (testBitwise)
import TestCheckMultiSig (testCheckMultiSig)
import TestCheckSig (testCheckSig)
import TestCodeMetrics (testCodeMetrics)
import TestConstants (testConstants)
import TestCustomTypes (testCustomTypes)
import TestDataPushing (testDataPushing)
import TestDataSig (testDataSig)
import TestEllipticCurve (testEllipticCurve)
import TestEllipticCurveNative (testEllipticCurveNative)
import TestEval (testEval)
import TestFunctions (testFunctions)
import TestFunctionsLowLevel (testFunctionsLowLevel)
import TestIntPushing (testIntPushing)
import TestInteger (testInteger)
import TestIntrospection (testIntrospection)
import TestInvalidStack (testInvalidStack)
import TestLibConditionals (testLibConditionals)
import TestLibEither (testLibEither)
import TestLibMaybe (testLibMaybe)
import TestLibMisc (testLibMisc)
import TestLibTuple (testLibTuple)
import TestLibVector (testLibVector)
import TestLibVectorAlgorithms (testLibVectorAlgorithms)
import TestLibauthVectors2025 (testLibauthVectors2025)
import TestLibauthVectors2026 (testLibauthVectors2026)
import TestLibauthVectorsSpec (testLibauthVectorsSpec)
import TestLoops (testLoops)
import TestLzss (testLzss)
import TestLzssBit (testLzssBit)
import TestOpcodes (testOpcodes)
import TestOpsArithmetic (testOpsArithmetic)
import TestOpsBytes (testOpsBytes)
import TestOpsConditional (testOpsConditional)
import TestOpsHash (testOpsHash)
import TestOpsOrdering (testOpsOrdering)
import TestOpsStack (testOpsStack)
import TestOptimizer (testOptimizer)
import TestQuotationsA (testQuotationsA)
import TestQuotationsB (testQuotationsB)
import TestRuntimeLib (testRuntimeLib)
import TestStackBranches (testStackBranches)
import TestTurtleVm2025 (testTurtleVm2025)
import TestTurtleVm2026 (testTurtleVm2026)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testArguments,
      testBitwise,
      testCheckMultiSig,
      testCheckSig,
      testCodeMetrics,
      testConstants,
      testCustomTypes,
      testDataPushing,
      testDataSig,
      testEllipticCurve,
      testEllipticCurveNative,
      testEval,
      testFunctions,
      testFunctionsLowLevel,
      testIntPushing,
      testInteger,
      testIntrospection,
      testInvalidStack,
      testQuotationsA,
      testQuotationsB,
      testLibConditionals,
      testLibEither,
      testLibTuple,
      testLibMaybe,
      testLibMisc,
      testLibVector,
      testLibVectorAlgorithms,
      testLibauthVectors2025,
      testLibauthVectors2026,
      testLibauthVectorsSpec,
      testLoops,
      testLzss,
      testLzssBit,
      testOpcodes,
      testOpsArithmetic,
      testOpsBytes,
      testOpsConditional,
      testOpsHash,
      testOpsOrdering,
      testOpsStack,
      testOptimizer,
      testStackBranches,
      testRuntimeLib,
      testTurtleVm2025,
      testTurtleVm2026
    ]
