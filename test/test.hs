-- Copyright (c) 2025 albaDsl

import Test.Tasty (TestTree, defaultMain, testGroup)
import TestArguments (testArguments)
import TestBitwise (testBitwise)
import TestCheckMultiSig (testCheckMultiSig)
import TestCheckSig (testCheckSig)
import TestCodeMetrics (testCodeMetrics)
import TestCond (testCond)
import TestConstants (testConstants)
import TestCustomTypes (testCustomTypes)
import TestDataPushing (testDataPushing)
import TestDataSig (testDataSig)
import TestFunctions (testFunctions)
import TestFunctionsLowLevel (testFunctionsLowLevel)
import TestIntPushing (testIntPushing)
import TestInteger (testInteger)
import TestIntrospection (testIntrospection)
import TestInvalidStack (testInvalidStack)
import TestLambdas (testLambdas)
import TestLibMaybe (testLibMaybe)
import TestLibTuple (testLibTuple)
import TestLibVector (testLibVector)
import TestLibauthVectors2025 (testLibauthVectors2025)
import TestLibauthVectors2026 (testLibauthVectors2026)
import TestLookupTables (testLookupTables)
import TestLoops (testLoops)
import TestOpcodes (testOpcodes)
import TestOpsArithmetic (testOpsArithmetic)
import TestOpsBytes (testOpsBytes)
import TestOpsConditional (testOpsConditional)
import TestOpsHash (testOpsHash)
import TestOpsOrdering (testOpsOrdering)
import TestOpsStack (testOpsStack)
import TestOptimizer (testOptimizer)
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
      testCond,
      testConstants,
      testCustomTypes,
      testDataPushing,
      testDataSig,
      testLookupTables,
      testFunctions,
      testFunctionsLowLevel,
      testIntPushing,
      testInteger,
      testIntrospection,
      testInvalidStack,
      testLambdas,
      testLibTuple,
      testLibMaybe,
      testLibVector,
      testLibauthVectors2025,
      testLibauthVectors2026,
      testLoops,
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
