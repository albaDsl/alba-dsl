-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.VmOpBytes (evalOpBytes) where

import Alba.Misc.Utils (maybeToEither)
import Alba.Vm.Bch2025.Utils (ba1, ba2, nc1, op1, op2)
import Alba.Vm.Bch2025.Utils qualified as VU
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement
  ( StackElement (..),
    bytesToStackElement,
    integerToStackElement,
    stackElementToBytes,
    stackElementToInteger,
    stackElementToInteger',
    verifyStackElementSize,
  )
import Alba.Vm.Common.VmInteger (extendToByteSize)
import Alba.Vm.Common.VmLimits (addBytesPushed)
import Alba.Vm.Common.VmParams (VmParams (..))
import Alba.Vm.Common.VmState (VmState (..))
import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.Sequence (Seq ((:|>)), (|>))

{- ORMOLU_DISABLE -}
evalOpBytes :: OpcodeL2 -> VmState -> Maybe (Either ScriptError VmState)
evalOpBytes op st@VmState {params, s} =
  case op of
    OP_SIZE -> Just $ do
      (_ :|> bytes) <- pure s
      let bytes' = stackElementToBytes bytes
      res <- integerToStackElement params (fromIntegral (B.length bytes'))
      Right $ addBytesPushed res.byteSize (st {s = s |> res})
    OP_SPLIT -> Just $ do
      (s' :|> bytes :|> n) <- pure s
      let bytes' = stackElementToBytes bytes
      n' <- fromIntegral <$> stackElementToInteger params n
      unless (n' >= 0 && n' <= B.length bytes') (Left SeInvalidSplitRange)
      let (x1, x2) = B.splitAt n' bytes'
      x1' <- bytesToStackElement params x1
      x2' <- bytesToStackElement params x2
      Right $
        addBytesPushed
          (x1'.byteSize + x2'.byteSize)
          (st {s = s' |> x1' |> x2'})
    OP_CAT ->          op2 st ((br2 . ba2 . asc params) B.append)
    OP_NUM2BIN ->      op2 st ((br2 . bia2) (encodeInBytes params))
    OP_BIN2NUM ->      op1 st ((ir1 . ia1' . nc1) id) -- FIXME: performance
    OP_REVERSEBYTES -> op1 st ((br1 . ba1 . nc1) B.reverse)
    _ -> Nothing
  where
    bia2 = VU.bia2 params
    br1 = VU.br1 params
    br2 = VU.br2 params
    ir1 = VU.ir1 params

    -- Integer Argument. Without the minimal encoding check.
    ia1' f x =
      stackElementToInteger' False params.maxIntegerBytes params x >>= f

    -- Append size check.
    asc vmParams f x y =
      verifyStackElementSize vmParams (B.length x + B.length y) >> Right (f x y)

    encodeInBytes vmParams bytes n = do
      let n' = fromIntegral n
      verifyStackElementSize vmParams n'
      maybeToEither SeImpossibleEncoding (extendToByteSize bytes n')
{- ORMOLU_ENABLE -}
