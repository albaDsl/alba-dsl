-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.LzssBit where

import Alba.Dsl.V1.Bch2026
  ( FN,
    Loop,
    TBool,
    TBytes,
    TInt,
    TNat,
    begin,
    bytes,
    cast,
    function,
    nat,
    ns2,
    ns3,
    op1Add,
    opAdd,
    opAnd,
    opBin2Num,
    opCat,
    opFalse,
    opIf,
    opLessThanOrEqual,
    opMod,
    opNumEqual,
    opRShiftNum,
    opSize,
    opSplit,
    opSubUnsafe,
    opTrue,
    opUntil,
    pick,
    roll,
    un3,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, nip, rot, swap)
import Numeric.Natural (Natural)
import Prelude ()

type TRef = TBytes

type TLen = TNat

type TOff = TNat

-- Experiment with type aliases.
{- ORMOLU_DISABLE -}
type Acc = "acc"
type Bs = "bs"
type Bs' = "bs'"
type End = "end"
type I = "i"
type Len = "len"
type Off = "off"
type Start = "start"
{- ORMOLU_ENABLE -}

padLen, refLen, lenBits, lenBias, offBias :: Natural
padLen = 2 -- Byte size of padding.
refLen = 2 -- Byte size of a reference.
lenBits = 4 -- Number of bits used to store the match length in the ref.
lenBias = 3 -- Bias to add to the length field.
offBias = 1 -- Bias to add to the offset field.

-- >>> import Alba.Dsl.V1.Bch2026 qualified as Dsl
-- >>> Dsl.progSize decompress
-- "2 opcodes, 2 bytes. Including function table: 5 opcodes, 100 bytes.\n"
decompress :: FN (s > TBytes) (s > TBytes)
decompress = function (pad # bytes [] # opUntil decompressLoop # nip)
  where
    -- Keeps the bytestring positive (when viewed as a number).
    pad = bytes [0xff, 0x00] # opCat

decompressLoop :: Loop (s > TBytes > TBytes) -- bs acc
decompressLoop =
  begin
    # (swap # opSize # nat padLen # opLessThanOrEqual)
    # opIf
      (swap # opTrue)
      ( begin
          # ns2 Acc Bs
          # (pick Bs # lowBitSet # roll Bs # dropLowBit # swap)
          # opIf
            (getBits8 # roll Acc # rot # opCat # opFalse)
            ( begin
                # (getBits16 # swap # unpackRef) -- <acc> bs off len
                # (roll Acc # rot # rot # copyFromBack # opFalse)
            )
      )
  where
    lowBitSet :: FN (s > TBytes) (s > TBool)
    lowBitSet = cast # nat 2 # opMod # nat 1 # opNumEqual

    dropLowBit :: FN (s > TBytes) (s > TBytes)
    dropLowBit = cast # nat 1 # opRShiftNum # cast

    getBits8 :: FN (s > TBytes) (s > TBytes > TBytes)
    getBits8 = nat 1 # opSplit

    getBits16 :: FN (s > TBytes) (s > TBytes > TBytes)
    getBits16 = nat 2 # opSplit

unpackRef :: FN (s > TBytes) (s > TOff > TLen)
unpackRef =
  begin
    # (dup # toSigned # nat lenBits # opRShiftNum # i2n # nat offBias # opAdd)
    # (swap # maskLen # opAnd # opBin2Num # i2n # nat lenBias # opAdd)
  where
    toSigned :: FN (s > TBytes) (s > TInt)
    toSigned = bytes [0] # opCat # opBin2Num

    i2n :: FN (s > TInt) (s > TNat)
    i2n = cast

    maskLen = bytes [0x0f, 0x00]

-- This implementation is a bit cost inefficient. A further cost optimized
-- version of it gave a 23% reduction in cost at a 27% increase in decompressor
-- size. We have kept the cost inefficient version to prioritize code size and
-- simplicity.
copyFromBack :: FN (s > TBytes > TNat > TNat) (s > TBytes) -- bs off len
copyFromBack =
  begin
    # (ns3 Bs Off Len # roll Bs # opSize # roll Off # opSubUnsafe)
    # (dup # roll Len # opAdd # rot # opUntil loop # nip # nip)
  where
    loop :: Loop (s > TNat > TNat > TBytes) -- start end acc
    loop =
      begin
        # (ns3 I End Acc # pick I # pick End # opNumEqual)
        # opIf
          (un3 I End Acc # opTrue)
          ( begin
              # (pick I # op1Add # roll End)
              # (roll Acc # dup # roll I # index # opCat # opFalse)
          )

    index :: FN (s > TBytes > TNat) (s > TBytes)
    index = opSplit # nip # nat 1 # opSplit # drop
