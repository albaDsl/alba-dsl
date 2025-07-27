-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2026.TurtleVm (turtleVm) where

import Alba.Dsl.V1.Bch2025.OpsUntyped
  ( op0,
    opCheckDataSig,
    opCheckDataSigVerify,
    opCheckLockTimeVerify,
    opCheckSequenceVerify,
    opDrop,
    opIf,
    opReverseBytes,
  )
import Alba.Dsl.V1.Bch2026 qualified as TY
import Alba.Dsl.V1.Bch2026.OpsUntyped (opUntil)
import Alba.Dsl.V1.Common.Lang (begin, (#))
import Alba.Dsl.V1.Common.StackUntyped (FNU, fromTyped)
import DslDemo.TurtleVm.Bch2026.TurtleOpArithmetic (turtleOpArithmetic)
import DslDemo.TurtleVm.Bch2026.TurtleOpBitwiseLogic (turtleOpBitwiseLogic)
import DslDemo.TurtleVm.Bch2026.TurtleOpBytes (turtleOpBytes)
import DslDemo.TurtleVm.Bch2026.TurtleOpConditionals (turtleOpConditionals)
import DslDemo.TurtleVm.Bch2026.TurtleOpConstants (turtleOpConstants)
import DslDemo.TurtleVm.Bch2026.TurtleOpCryptography (turtleOpCryptography)
import DslDemo.TurtleVm.Bch2026.TurtleOpIntrospection (turtleOpIntrospection)
import DslDemo.TurtleVm.Bch2026.TurtleOpStack (turtleOpStack)
import DslDemo.TurtleVm.Bch2026.TurtleVmCondStack (executeP)
import DslDemo.TurtleVm.Bch2026.TurtleVmState (getOpAndCondStack, getOpBytes, initState, isEndOfProgram)
import DslDemo.TurtleVm.Bch2026.TurtleVmUtils (vmError)
import DslDemo.TurtleVm.Bch2026.TurtleVmUtilsUntyped (condOp, inRange, is)

turtleVm :: Int -> FNU
turtleVm maxCsDepth =
  begin
    # ft initState
    # opUntil loop
  where
    loop :: FNU
    loop =
      begin
        # (ft getOpAndCondStack # ft executeP)
        # opIf (handleOp maxCsDepth) opDrop
        # ft isEndOfProgram

ft :: TY.FNA s alt s' alt' -> FNU
ft = fromTyped

handleOp :: Int -> FNU
handleOp maxCsDepth =
  condOp
    [ ( inRange 0x00 0x83,
        condOp
          [ (is 0x00, opDrop # op0),
            (inRange 0x01 0x4c, ft getOpBytes),
            (inRange 0x4c 0x4f, ft (vmError "E5")), -- OP_PUSHDATA
            ( inRange 0x4f 0x83,
              condOp
                [ (inRange 0x4f 0x61, turtleOpConstants),
                  (inRange 0x61 0x6b, turtleOpConditionals maxCsDepth),
                  (inRange 0x6b 0x7e, turtleOpStack),
                  (inRange 0x7e 0x83, turtleOpBytes)
                ]
            )
          ]
      ),
      ( inRange 0x83 0xd5,
        condOp
          [ (inRange 0x83 0x89, turtleOpBitwiseLogic),
            -- 0x89 = OP_RESERVED1_OP_DEFINE
            -- 0x8A = OP_RESERVED2_OP_INVOKE
            (inRange 0x8b 0xa6, turtleOpArithmetic),
            (inRange 0xa6 0xb0, turtleOpCryptography),
            -- 0xB0 = OP_NOP1
            (is 0xb1, opDrop # opCheckLockTimeVerify),
            (is 0xb2, opDrop # opCheckSequenceVerify),
            -- 0xB3 - 0xB9 = discouraged nops
            (is 0xba, opDrop # opCheckDataSig),
            (is 0xbb, opDrop # opCheckDataSigVerify),
            (is 0xbc, opDrop # opReverseBytes),
            -- 0xBD = OP_AVAILABLE_BD
            -- 0xBE = OP_AVAILABLE_BE
            -- 0xBF = OP_AVAILABLE_BF
            (inRange 0xc0 0xd4, turtleOpIntrospection)
          ]
      )
    ]
