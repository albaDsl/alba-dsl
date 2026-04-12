-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.LzssBit where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Loop,
    TBool,
    TBytes,
    TInt,
    TNat,
    begin,
    bytes,
    cast,
    fn,
    nat,
    ns2,
    ns3,
    op1Add,
    op2Drop,
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
-- "2 opcodes, 2 bytes. Including function table: 5 opcodes, 92 bytes.\n"
decompress :: Fn (s > TBytes) (s > TBytes)
decompress = fn (bytes [] # swap # pad # opUntil decompressLoop # drop)
  where
    -- Keeps the bytestring positive (when viewed as a number).
    pad = bytes [0xff, 0x00] # opCat

decompressLoop :: Loop (s > TBytes > TBytes) -- acc bs
decompressLoop =
  begin
    # (opSize # nat padLen # opLessThanOrEqual)
    # opIf
      opTrue
      ( begin
          # ns2 Acc Bs
          # (pick Bs # dropLowBit # roll Bs # lowBitSet)
          # opIf
            (getBits8 # roll Acc # rot # opCat)
            ( begin
                # (getBits16 # swap # unpackRef) -- <acc> bs off len
                # (roll Acc # rot # rot # copyFromBack)
            )
          # (swap # opFalse)
      )
  where
    lowBitSet :: Fn (s > TBytes) (s > TBool)
    lowBitSet = b2n # nat 2 # opMod # nat 1 # opNumEqual

    b2n :: Fn (s > TBytes) (s > TNat)
    b2n = cast

    dropLowBit :: Fn (s > TBytes) (s > TBytes)
    dropLowBit = b2i # nat 1 # opRShiftNum # cast

    b2i :: Fn (s > TBytes) (s > TInt)
    b2i = cast

    getBits8 :: Fn (s > TBytes) (s > TBytes > TBytes)
    getBits8 = nat 1 # opSplit

    getBits16 :: Fn (s > TBytes) (s > TBytes > TBytes)
    getBits16 = nat 2 # opSplit

unpackRef :: Fn (s > TBytes) (s > TOff > TLen)
unpackRef =
  begin
    # (dup # toSigned # nat lenBits # opRShiftNum # i2n # nat offBias # opAdd)
    # (swap # maskLen # opAnd # opBin2Num # i2n # nat lenBias # opAdd)
  where
    toSigned :: Fn (s > TBytes) (s > TInt)
    toSigned = bytes [0] # opCat # opBin2Num

    i2n :: Fn (s > TInt) (s > TNat)
    i2n = cast

    maskLen = bytes [0x0f, 0x00]

-- This implementation is a bit cost inefficient. A further cost optimized
-- version of it gave a 23% reduction in cost at a 27% increase in decompressor
-- size. We have kept the cost inefficient version to prioritize code size and
-- simplicity.
copyFromBack :: Fn (s > TBytes > TNat > TNat) (s > TBytes) -- bs off len
copyFromBack =
  begin
    # (ns3 Bs Off Len # roll Bs # opSize # roll Off # opSubUnsafe)
    # (dup # roll Len # opAdd # opUntil loop # op2Drop)
  where
    loop :: Loop (s > TBytes > TNat > TNat) -- acc start end
    loop =
      begin
        # (ns3 Acc I End # pick I # pick End # opNumEqual)
        # opIf
          (un3 Acc I End # opTrue)
          ( begin
              # (roll Acc # dup # pick I # index # opCat)
              # (roll I # op1Add # roll End # opFalse)
          )

    index :: Fn (s > TBytes > TNat) (s > TBytes)
    index = opSplit # nip # nat 1 # opSplit # drop
