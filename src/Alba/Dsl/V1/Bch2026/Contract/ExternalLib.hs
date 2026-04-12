-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.ExternalLib
  ( importLibrary,
    importLibrary',
    simpleUnwrapProg,
  )
where

import Alba.Dsl.V1.Bch2026
  ( Fn,
    Loop,
    TBytes,
    TCode,
    THash256,
    TInt,
    TLambda,
    TNat,
    begin,
    bytes,
    cast,
    fn,
    int,
    invoke1,
    n2i,
    name,
    nat,
    ns3,
    ns5,
    ns6,
    op1Add,
    op1Sub,
    opCat,
    opDefine,
    opEqualVerify,
    opHash256,
    opInvoke,
    opNumEqual,
    opSplit,
    opUntil,
    opUtxoBytecode,
    pick,
    roll,
    un,
    (.),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, nip)
import Alba.Dsl.V1.Bch2026.TxDsl (simpleWrapChunkSize)
import Prelude (fromIntegral, undefined)

importLibrary ::
  Fn
    (s > TBytes > TLambda '[TBytes] '[TBytes] > TNat > TNat > TNat > THash256)
    s
importLibrary =
  fn
    ( begin
        . ns6 #fId #transform #startInput #numInputs #size #hash
        . (roll #startInput . roll #numInputs . n2i . bytes [])
        . (opUntil loop . nip . nip)
        . (roll #size . opSplit . drop . roll #transform . invoke1)
        . (dup . opHash256 . roll #hash . opEqualVerify . b2c)
        . (pick #fId . opDefine . roll #fId . opInvoke libInit)
    )
  where
    loop :: Loop (s > TNat > TInt > TBytes)
    loop =
      begin
        . ns3 #input #cnt #acc
        . (pick #input . op1Add)
        . name #cnt' (roll #cnt . op1Sub)
        . (roll #acc . roll #input . opUtxoBytecode . simpleUnwrapProg)
        . (opCat . pick #cnt' . int 0 . opNumEqual . un #cnt')

    libInit :: Fn s s
    libInit = undefined

b2c :: Fn (s > TBytes) (s > TCode)
b2c = cast

-- Without transform.
importLibrary' ::
  Fn
    (s > TBytes > TNat > TNat > TNat > THash256)
    s
importLibrary' =
  fn
    ( begin
        . ns5 #fId #startInput #numInputs #size #hash
        . (roll #startInput . roll #numInputs . n2i . bytes [])
        . (opUntil loop . nip . nip)
        . (roll #size . opSplit . drop)
        . (dup . opHash256 . roll #hash . opEqualVerify . b2c)
        . (pick #fId . opDefine . roll #fId . opInvoke libInit)
    )
  where
    loop :: Loop (s > TNat > TInt > TBytes)
    loop =
      begin
        . ns3 #input #cnt #acc
        . (pick #input . op1Add)
        . name #cnt' (roll #cnt . op1Sub)
        . (roll #acc . roll #input . opUtxoBytecode . simpleUnwrapProg)
        . (opCat . pick #cnt' . int 0 . opNumEqual . un #cnt')

    libInit :: Fn s s
    libInit = undefined

-- Unwraps the 'simpleWrap' format from Bch2026.TxDsl. With simple wrapping the
-- data chunk always starts at offset 2 and is 197 bytes long.
simpleUnwrapProg :: Fn (s > TBytes) (s > TBytes)
simpleUnwrapProg =
  fn (nat 2 . opSplit . nip . nat size . opSplit . drop)
  where
    size = fromIntegral simpleWrapChunkSize
