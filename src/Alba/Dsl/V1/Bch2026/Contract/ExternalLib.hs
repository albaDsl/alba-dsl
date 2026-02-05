-- Copyright (c) 2026 albaDsl

module Alba.Dsl.V1.Bch2026.Contract.ExternalLib
  ( importLibrary,
    importLibrary',
    simpleUnwrapProg,
  )
where

import Alba.Dsl.V1.Bch2025
  ( FN,
    TBytes,
    THash256,
    TInt,
    TNat,
    begin,
    bytes,
    cast,
    int,
    name,
    nat,
    ns3,
    ns5,
    ns6,
    op1Add,
    op1Sub,
    opCat,
    opEqual,
    opEqualVerify,
    opHash256,
    opSplit,
    opUtxoBytecode,
    pick,
    roll,
    un,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.Shorthand (drop, dup, nip)
import Alba.Dsl.V1.Bch2026.Lang (function, invoke1)
import Alba.Dsl.V1.Bch2026.LangArgs (Loop)
import Alba.Dsl.V1.Bch2026.Ops (opDefine, opInvoke, opUntil)
import Alba.Dsl.V1.Bch2026.Stack (TLambda)
import Alba.Dsl.V1.Bch2026.TxDsl (simpleWrapChunkSize)
import Prelude hiding (drop)

importLibrary ::
  FN
    (s > TBytes > TLambda '[TBytes] '[TBytes] > TNat > TNat > TNat > THash256)
    s
importLibrary =
  function
    ( begin
        # ns6 "fId" "transform" "startInput" "numInputs" "size" "hash"
        # (roll "startInput" # roll "numInputs" # n2i # bytes [])
        # (opUntil loop # nip # nip)
        # (roll "size" # opSplit # drop # roll "transform" # invoke1)
        # (dup # opHash256 # roll "hash" # opEqualVerify)
        # (pick "fId" # opDefine # roll "fId" # opInvoke libInit)
    )
  where
    loop :: Loop (s > TNat > TInt > TBytes)
    loop =
      begin
        # ns3 "input" "cnt" "acc"
        # (pick "input" # op1Add)
        # name "cnt'" (roll "cnt" # op1Sub)
        # (roll "acc" # roll "input" # opUtxoBytecode # simpleUnwrapProg)
        # (opCat # pick "cnt'" # int 0 # opEqual # un "cnt'")

    libInit :: FN s s
    libInit = undefined

    n2i :: FN (s > TNat) (s > TInt)
    n2i = cast

-- Without transform.
importLibrary' ::
  FN
    (s > TBytes > TNat > TNat > TNat > THash256)
    s
importLibrary' =
  function
    ( begin
        # ns5 "fId" "startInput" "numInputs" "size" "hash"
        # (roll "startInput" # roll "numInputs" # n2i # bytes [])
        # (opUntil loop # nip # nip)
        # (roll "size" # opSplit # drop)
        # (dup # opHash256 # roll "hash" # opEqualVerify)
        # (pick "fId" # opDefine # roll "fId" # opInvoke libInit)
    )
  where
    loop :: Loop (s > TNat > TInt > TBytes)
    loop =
      begin
        # ns3 "input" "cnt" "acc"
        # (pick "input" # op1Add)
        # name "cnt'" (roll "cnt" # op1Sub)
        # (roll "acc" # roll "input" # opUtxoBytecode # simpleUnwrapProg)
        # (opCat # pick "cnt'" # int 0 # opEqual # un "cnt'")

    libInit :: FN s s
    libInit = undefined

    n2i :: FN (s > TNat) (s > TInt)
    n2i = cast

-- Unwraps the 'simpleWrap' format from Bch2026.TxDsl. With simple wrapping the
-- data chunk always starts at offset 2 and is 197 bytes long.
simpleUnwrapProg :: FN (s > TBytes) (s > TBytes)
simpleUnwrapProg =
  function (nat 2 # opSplit # nip # nat size # opSplit # drop)
  where
    size = fromIntegral simpleWrapChunkSize
