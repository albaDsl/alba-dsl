-- Copyright (c) 2025 albaDsl

module DslDemo.TurtleVm.Bch2025.TurtleVm (turtleVm) where

import Alba.Dsl.V1.Bch2025 qualified as TY
import Alba.Dsl.V1.Bch2025.LangUntyped (int, repeatProg)
import Alba.Dsl.V1.Bch2025.OpsUntyped
  ( op0,
    opCheckDataSig,
    opCheckDataSigVerify,
    opCheckLockTimeVerify,
    opCheckSequenceVerify,
    opDrop,
    opIf,
    opReverseBytes,
    opSplit,
    opSwap,
    opUnless,
  )
import Alba.Dsl.V1.Common.Lang (begin, (∘))
import Alba.Dsl.V1.Common.StackUntyped (FnU, fromTyped)
import DslDemo.TurtleVm.Bch2025.TurtleOpArithmetic (turtleOpArithmetic)
import DslDemo.TurtleVm.Bch2025.TurtleOpBitwiseLogic (turtleOpBitwiseLogic)
import DslDemo.TurtleVm.Bch2025.TurtleOpBytes (turtleOpBytes)
import DslDemo.TurtleVm.Bch2025.TurtleOpConditionals (turtleOpConditionals)
import DslDemo.TurtleVm.Bch2025.TurtleOpConstants (turtleOpConstants)
import DslDemo.TurtleVm.Bch2025.TurtleOpCryptography (turtleOpCryptography)
import DslDemo.TurtleVm.Bch2025.TurtleOpIntrospection (turtleOpIntrospection)
import DslDemo.TurtleVm.Bch2025.TurtleOpStack (turtleOpStack)
import DslDemo.TurtleVm.Bch2025.TurtleVmCondStack (executeP)
import DslDemo.TurtleVm.Bch2025.TurtleVmState (getOpAndCondStack, initState)
import DslDemo.TurtleVm.Bch2025.TurtleVmUtils
  ( isOpDataOp,
    isSingleByteOp,
    toSigned,
    vmError,
  )
import DslDemo.TurtleVm.Bch2025.TurtleVmUtilsUntyped
  ( condOp,
    inRange,
    is,
    unsupportedOp,
  )

turtleVm :: Int -> Int -> FnU
turtleVm maxOps maxCsDepth =
  ft initState ∘ repeatProg maxOps (handleOp maxCsDepth)

ft :: TY.FnA s alt s' alt' -> FnU
ft = fromTyped

handleOp :: Int -> FnU
handleOp maxCsDepth =
  begin
    ∘ (ft getOpAndCondStack ∘ ft (executeP maxCsDepth))
    ∘ opIf (handleOp' maxCsDepth) opDrop

handleOp' :: Int -> FnU
handleOp' maxCsDepth =
  begin
    ∘ ft isSingleByteOp
    ∘ opIf
      ( begin
          ∘ ft toSigned
          ∘ condOp
            [ ( inRange 0x00 0x83,
                condOp
                  [ (is 0x00, opDrop ∘ op0),
                    -- (inRange 0x01 0x4c, ft getOpBytes),
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
                    (is 0xb1, opDrop ∘ opCheckLockTimeVerify),
                    (is 0xb2, opDrop ∘ opCheckSequenceVerify),
                    -- 0xB3 - 0xB9 = discouraged nops
                    (is 0xba, opDrop ∘ opCheckDataSig),
                    (is 0xbb, opDrop ∘ opCheckDataSigVerify),
                    (is 0xbc, opDrop ∘ opReverseBytes),
                    -- 0xBD = OP_AVAILABLE_BD
                    -- 0xBE = OP_AVAILABLE_BE
                    -- 0xBF = OP_AVAILABLE_BF
                    (inRange 0xc0 0xd4, turtleOpIntrospection)
                  ]
              )
            ]
      )
      ( begin
          ∘ (int 1 ∘ opSplit ∘ opSwap)
          ∘ ft isOpDataOp
          ∘ opUnless unsupportedOp
      )
