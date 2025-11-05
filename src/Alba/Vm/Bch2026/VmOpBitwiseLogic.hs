-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2026.VmOpBitwiseLogic (evalOpBitwiseLogic) where

import Alba.Vm.Bch2025.Utils (ba1, nc1, op1, op2)
import Alba.Vm.Bch2025.Utils qualified as VU
import Alba.Vm.Common.OpcodeL2 (OpcodeL2 (..))
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (Bytes)
import Alba.Vm.Common.VmState (VmState (..))
import Data.Bits (complement, shiftL, shiftR, (.|.))
import Data.ByteString qualified as B
import Data.Int (Int32)
import Data.Word (Word8)

evalOpBitwiseLogic ::
  OpcodeL2 ->
  VmState ->
  Maybe (Either ScriptError VmState)
evalOpBitwiseLogic op st@(VmState {params}) =
  case op of
    OP_INVERT -> op1 st ((br1 . ba1 . nc1) (B.map complement))
    OP_LSHIFTNUM -> op2 st ((ir2 . ia2 . cs2) lshiftNum)
    OP_RSHIFTNUM -> op2 st ((ir2 . ia2 . cs2) rshiftNum)
    OP_LSHIFTBIN -> op2 st ((br2 . bia2 . cs2) lshiftBin)
    OP_RSHIFTBIN -> op2 st ((br2 . bia2 . cs2) rshiftBin)
    _ -> Nothing
  where
    ia2 = VU.ia2 params
    bia2 = VU.bia2 params
    ir2 = VU.ir2 params
    br1 = VU.br1 params
    br2 = VU.br2 params

    -- Count check and clamp. Error if the bit count is negative. Also,
    -- clamp the bit count to the maximum value for an Int32.
    cs2 :: (a -> Integer -> a) -> (a -> Integer -> Either ScriptError a)
    cs2 _f _num shift | shift < 0 = Left SeInvalidBitShift
    cs2 f num shift =
      let shift' = min shift (fromIntegral (maxBound :: Int32))
       in Right $ f num shift'

    lshiftNum :: Integer -> Integer -> Integer
    lshiftNum num shift = shiftL num (fromIntegral shift)

    rshiftNum :: Integer -> Integer -> Integer
    rshiftNum num shift = shiftR num (fromIntegral shift)

lshiftBin :: Bytes -> Integer -> Bytes
lshiftBin bytes 0 = bytes
lshiftBin "" _ = B.empty
lshiftBin bytes count
  | B.length bytes * 8 <= fromIntegral count =
      B.replicate (B.length bytes) 0
lshiftBin bytes count =
  let byteCount = fromIntegral count `div` 8
      bitCount = fromIntegral count `mod` 8
      zeroes = B.replicate byteCount 0
   in shiftBits (B.drop byteCount bytes) bitCount <> zeroes
  where
    shiftBits :: Bytes -> Int -> Bytes
    shiftBits bytes' count' =
      let zero = B.singleton 0
       in B.pack . drop 1 $
            B.zipWith (combine count') (zero <> bytes') (bytes' <> zero)

    combine :: Int -> Word8 -> Word8 -> Word8
    combine cnt word1 word2 = shiftL word1 cnt .|. shiftR word2 (8 - cnt)

rshiftBin :: Bytes -> Integer -> Bytes
rshiftBin bytes 0 = bytes
rshiftBin "" _ = B.empty
rshiftBin bytes count
  | B.length bytes * 8 <= fromIntegral count =
      B.replicate (B.length bytes) 0
rshiftBin bytes count =
  let len = B.length bytes
      byteCount = fromIntegral count `div` 8
      bitCount = fromIntegral count `mod` 8
      zeroes = B.replicate byteCount 0
   in B.take len $ zeroes <> shiftBits bytes bitCount
  where
    shiftBits :: Bytes -> Int -> Bytes
    shiftBits bytes' count' =
      let zero = B.singleton 0
       in B.pack $ B.zipWith (combine count') (zero <> bytes') bytes'

    combine :: Int -> Word8 -> Word8 -> Word8
    combine cnt word1 word2 = shiftL word1 (8 - cnt) .|. shiftR word2 cnt
