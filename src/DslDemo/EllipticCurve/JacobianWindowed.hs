-- Copyright (c) 2025 albaDsl

module DslDemo.EllipticCurve.JacobianWindowed
  ( setupTable,
    ecDouble,
    ecAdd,
    ecMul,
  )
where

import Alba.Dsl.V1.Bch2026
import Alba.Vm.Common.OpcodeL1 (OpcodeL1 (..))
import DslDemo.EllipticCurve.Jacobian
  ( ecAdd,
    ecDouble,
    fromJacobian,
    toJacobian,
  )
import DslDemo.EllipticCurve.JacobianAdd qualified as EC
import DslDemo.EllipticCurve.JacobianPoint (TPointJ, makeIdentity)
import DslDemo.EllipticCurve.LookupTable (getConstant)
import DslDemo.EllipticCurve.Point (TPoint)
import Numeric.Natural (Natural)

type TFId = TNat -- Function Id

type TTab = TFId -- Lookup table (represented by the base Function Id)

setupTable :: Fn (s > TFId > TPoint) s
setupTable =
  toJacobian # makeIdentity # nat numValues # unname 4 setupTable'
  where
    setupTable' ::
      Fn (s > N "fId" TNat > N "p" TPointJ > N "acc" TPointJ > N "i" TNat) s
    setupTable' =
      fn
        ( begin
            # (pick "i" # op0 # opNumEqual)
            # opIf
              (del "fId" # del "p" # del "acc" # del "i")
              ( begin
                  # (pick "fId" # pick "acc" # p2b # storePoint)
                  # (roll "fId" # op1Add)
                  # pick "p"
                  # (roll "acc" # roll "p" # EC.ecAddJ)
                  # (roll "i" # op1SubUnsafe)
                  # unname 4 setupTable'
              )
        )

    p2b :: Fn (s > TPointJ) (s > TBytes)
    p2b = cast

    -- Since the record size is fixed we can avoid using 'toPushOp' as an
    -- optimization.
    storePoint :: Fn (s > TNat > TBytes) s
    storePoint =
      begin
        # (opcode OP_PUSHDATA1 # size # opRot # opCat # opCat)
        # (b2c # opSwap # n2b)
        # opDefine
      where
        size :: Fn s (s > TBytes)
        size = nat 100 # n2b

        n2b :: Fn (s > TNat) (s > TBytes)
        n2b = cast

    opcode :: OpcodeL1 -> Fn s (s > TBytes)
    opcode op = bytes [(fromIntegral . fromEnum) op]

    b2c :: Fn (s > TBytes) (s > TCode)
    b2c = cast

ecMul :: Fn (s > TTab > TNat) (s > TPoint)
ecMul = fn (unname 2 ecMulJ # fromJacobian)

ecMulJ :: Fn (s > N "tab" TTab > N "n" TNat) (s > TPointJ)
ecMulJ =
  begin
    # (pick "n" # nat 0 # opNumEqual)
    # opIf
      (del "tab" # del "n" # makeIdentity)
      ( begin
          # (roll "tab" # roll "n" # digits # makeIdentity)
          # (opUntil (unname 3 ecMulJLoop) # opNip # opNip)
      )

ecMulJLoop ::
  Fn
    (s > N "tab" TTab > N "arr" TBytes > N "q" TPointJ)
    (s > TTab > TBytes > TPointJ > TBool)
ecMulJLoop =
  begin
    # (pick "arr" # bytes [] # opEqual)
    # opIf
      (roll "tab" # roll "arr" # roll "q" # opTrue)
      ( begin
          # name "q'" (nat windowSize # roll "q" # doubleN)
          # name2
            "arr'"
            "digit"
            (roll "arr" # nat 1 # opSplit # opSwap # opBin2Num # i2n)
          # (pick "digit" # op0 # opGreaterThan)
          # opIf
            ( begin
                # (pick "tab" # roll "digit" # tableLookup)
                # (roll "q'" # EC.ecAddJ)
            )
            (del "digit" # roll "q'")
          # (roll "tab" # roll "arr'" # opRot # opFalse)
      )
  where
    tableLookup :: Fn (s > TTab > TNat) (s > TPointJ)
    tableLookup = opAdd # getConstant # b2p

    i2n :: Fn (s > TInt) (s > TNat)
    i2n = cast

    b2p :: Fn (s > TBytes) (s > TPointJ)
    b2p = cast

doubleN :: Fn (s > TNat > TPointJ) (s > TPointJ)
doubleN =
  opUntil
    ( begin
        # (opSwap # opDup # op0 # opNumEqual)
        # opIf
          (opSwap # opTrue)
          (op1SubUnsafe # opSwap # EC.ecDoubleJ # opFalse)
    )
    # opNip

digits :: Fn (s > TNat) (s > TBytes)
digits = bytes [] # opSwap # opUntil (unname 2 loop) # opDrop
  where
    loop :: Fn (s > N "arr" TBytes > N "n" TNat) (s > TBytes > TNat > TBool)
    loop =
      begin
        # (pick "n" # winMod # n2i # nat 1 # opNum2Bin)
        # (roll "arr" # opCat # roll "n" # winDiv)
        # (opDup # op0 # opNumEqual)

    winMod = nat numValues # opMod
    winDiv = nat numValues # opDiv

windowSize :: Natural
windowSize = 4

numValues :: Natural
numValues = 2 ^ windowSize
