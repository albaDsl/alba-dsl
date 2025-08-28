-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianWindowed
  ( setupTable,
    ecDouble,
    ecAdd,
    ecMul,
  )
where

import Alba.Dsl.V1.Bch2026
import DslDemo.EllipticCurve.Jacobian
  ( ecAdd,
    ecDouble,
    fromJacobian,
    toJacobian,
  )
import DslDemo.EllipticCurve.JacobianAdd qualified as EC
import DslDemo.EllipticCurve.JacobianPoint (TPointJ, makeIdentity)
import DslDemo.EllipticCurve.LookupTable (defineConstant, getConstant)
import DslDemo.EllipticCurve.Point (TPoint)
import Numeric.Natural (Natural)
import Prelude hiding (drop)

type TFIdx = TNat -- Index into the functions array.

type TTab = TFIdx -- Lookup table

setupTable :: FN (s > TFIdx > TPoint) s
setupTable =
  toJacobian # makeIdentity # nat numValues # unname @4 setupTable'
  where
    setupTable' ::
      FN (s > N "fIdx" TNat > N "p" TPointJ > N "acc" TPointJ > N "i" TNat) s
    setupTable' =
      function
        ( begin
            # (pick @"i" # op0 # opEqual)
            # opIf
              (drop @"fIdx" # drop @"p" # drop @"acc" # drop @"i")
              ( begin
                  # (pick @"acc" # p2b # pick @"fIdx" # defineConstant)
                  # (roll @"fIdx" # op1Add)
                  # pick @"p"
                  # (roll @"acc" # roll @"p" # EC.ecAddJ)
                  # (roll @"i" # op1SubUnsafe)
                  # unname @4 setupTable'
              )
        )

    p2b :: FN (s > TPointJ) (s > TBytes)
    p2b = cast

ecMul :: FN (s > TTab > TNat) (s > TPoint)
ecMul = function (unname @2 ecMulJ # fromJacobian)

ecMulJ :: FN (s > N "tab" TTab > N "n" TNat) (s > TPointJ)
ecMulJ =
  begin
    # (pick @"n" # nat 0 # opNumEqual)
    # opIf
      (drop @"tab" # drop @"n" # makeIdentity)
      ( begin
          # (roll @"tab" # roll @"n" # digits # makeIdentity)
          # (opUntil (unname @3 ecMulJLoop) # opNip # opNip)
      )

ecMulJLoop ::
  FN
    (s > N "tab" TTab > N "arr" TBytes > N "q" TPointJ)
    (s > TTab > TBytes > TPointJ > TBool)
ecMulJLoop =
  begin
    # (pick @"arr" # bytes [] # opEqual)
    # opIf
      (roll @"tab" # roll @"arr" # roll @"q" # opTrue)
      ( begin
          # name @"q'" (nat windowSize # roll @"q" # doubleN)
          # name2 @"arr'" @"digit"
            (roll @"arr" # nat 1 # opSplit # opSwap # opBin2Num # i2n)
          # (pick @"digit" # op0 # opGreaterThan)
          # opIf
            ( begin
                # (pick @"tab" # roll @"digit" # tableLookup)
                # (roll @"q'" # EC.ecAddJ)
            )
            (drop @"digit" # roll @"q'")
          # (roll @"tab" # roll @"arr'" # opRot # opFalse)
      )
  where
    tableLookup :: FN (s > TTab > TNat) (s > TPointJ)
    tableLookup = opAdd # getConstant # b2p

    i2n :: FN (s > TInt) (s > TNat)
    i2n = cast

    b2p :: FN (s > TBytes) (s > TPointJ)
    b2p = cast

doubleN :: FN (s > TNat > TPointJ) (s > TPointJ)
doubleN =
  opUntil
    ( begin
        # (opSwap # opDup # op0 # opNumEqual)
        # opIf
          (opSwap # opTrue)
          (op1SubUnsafe # opSwap # EC.ecDoubleJ # opFalse)
    )
    # opNip

digits :: FN (s > TNat) (s > TBytes)
digits = bytes [] # opSwap # opUntil (unname @2 loop) # opDrop
  where
    loop :: FN (s > N "arr" TBytes > N "n" TNat) (s > TBytes > TNat > TBool)
    loop =
      begin
        # (pick @"n" # winMod # n2i # nat 1 # opNum2Bin)
        # (roll @"arr" # opCat # roll @"n" # winDiv)
        # (opDup # op0 # opNumEqual)

    winMod = nat numValues # opMod
    winDiv = nat numValues # opDiv

    n2i :: FN (s > TNat) (s > TInt)
    n2i = cast

windowSize :: Natural
windowSize = 4

numValues :: Natural
numValues = 2 ^ windowSize
