-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.Lzss where

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
    castStack,
    cond,
    del,
    function,
    name2,
    nat,
    ns3,
    ns6,
    op0,
    op1Add,
    opAdd,
    opAnd,
    opBin2Num,
    opCat,
    opEqual,
    opFalse,
    opGreaterThanOrEqual,
    opIf,
    opNot,
    opNumEqual,
    opRShiftBin,
    opRShiftNum,
    opSize,
    opSplit,
    opSubUnsafe,
    opTrue,
    opUntil,
    opVerify,
    opWhen,
    pick,
    roll,
    un3,
    un6,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, nip, rot, swap)
import Numeric.Natural (Natural)
import Prelude ()

type TRef = TBytes

type TLen = TNat

type TOff = TNat

groupSize, minMatchLen, lenBits, refLen :: Natural
groupSize = 8
minMatchLen = 3
lenBits = 4 -- Number of bits used to store the match length in the ref.
refLen = 2 -- Byte size of a reference.

-- >>> import Alba.Dsl.V1.Bch2026 qualified as Dsl
-- >>> Dsl.progSize decompress
-- "2 opcodes, 2 bytes. Including function table: 8 opcodes, 194 bytes.\n"
decompress :: FN (s > TBytes) (s > TBytes)
decompress =
  function
    ( begin
        # (opSize # nat 0 # nat groupSize # bytes [] # bytes [])
        # opUntil decompressLoop
        # (nip # nip # nip # nip # nip)
    )

decompressLoop :: Loop (s > TBytes > TNat > TNat > TNat > TBytes > TBytes)
decompressLoop =
  begin
    # ns6 "bs" "n" "i" "k" "flag" "out"
    # cond
      [ ( pick "i" # pick "n" # opGreaterThanOrEqual,
          opTrue # un6 "bs" "n" "i" "k" "flag" "out"
        ),
        ( pick "k" # nat groupSize # opEqual,
          begin
            # (pick "bs" # roll "n" # pick "i" # op1Add)
            # (del "k" # nat 0 # del "flag" # roll "bs")
            # (roll "i" # index # roll "out" # opFalse)
        ),
        ( pick "flag" # pick "k" # testBit,
          begin
            # (pick "bs" # roll "n" # pick "i" # op1Add)
            # (roll "k" # op1Add # roll "flag" # roll "out")
            # (roll "bs" # roll "i" # index # opCat # opFalse)
        )
      ]
      ( begin
          # (pick "i" # op1Add # pick "n" # opGreaterThanOrEqual)
          # opWhen (opFalse # opVerify # castStack)
          # name2 "off" "len" (pick "bs" # pick "i" # indexRef # unpackRef)
          # (roll "bs" # roll "n" # roll "i" # nat refLen # opAdd)
          # (roll "k" # op1Add # roll "flag")
          # (roll "out" # roll "off" # roll "len" # copyFromBack # opFalse)
      )

copyFromBack :: FN (s > TBytes > TNat > TNat) (s > TBytes)
copyFromBack =
  begin
    # (ns3 "bs" "off" "len" # roll "bs" # opSize # roll "off" # opSubUnsafe)
    # (dup # roll "len" # opAdd # rot # opUntil loop # nip # nip)
  where
    loop :: Loop (s > TNat > TNat > TBytes) -- start end acc
    loop =
      begin
        # (ns3 "i" "end" "acc" # pick "i" # pick "end" # opNumEqual)
        # opIf
          (un3 "i" "end" "acc" # opTrue)
          ( begin
              # (pick "i" # op1Add # roll "end")
              # (roll "acc" # dup # roll "i" # index # opCat # opFalse)
          )

index :: FN (s > TBytes > TNat) (s > TBytes)
index = function (opSplit # nip # nat 1 # opSplit # drop)

-- Shaves off a reference (two bytes) starting at the index.
indexRef :: FN (s > TBytes > TNat) (s > TBytes)
indexRef = opSplit # nip # nat refLen # opSplit # drop

-- TBytes is expected to be a single byte.
testBit :: FN (s > TBytes > TNat) (s > TBool)
testBit = opRShiftBin # bytes [0x01] # opAnd # opBin2Num # op0 # opEqual # opNot

-- Reference format: |offset:12|len:4|
unpackRef :: FN (s > TBytes) (s > TOff > TLen)
unpackRef =
  begin
    # (dup # toSigned # nat lenBits # opRShiftNum # i2n # op1Add)
    # (swap # nat 1 # opSplit # drop # bytes [0xf] # opAnd # opBin2Num # i2n)
    # (nat minMatchLen # opAdd)
  where
    toSigned :: FN (s > TBytes) (s > TInt)
    toSigned = bytes [0] # opCat # opBin2Num

i2n :: FN (s > TInt) (s > TNat)
i2n = cast
