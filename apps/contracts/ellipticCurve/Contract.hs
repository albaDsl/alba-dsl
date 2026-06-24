-- Copyright (c) 2026 albaDsl

module Contract (EllipticCurve, contract) where

import Alba.Dsl.V1.Bch2026
  ( CFn,
    Fn,
    Stack (Base, (:>)),
    TBytes,
    TNat,
    begin,
    constant,
    int,
    opBin2Num,
    runEnv,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.Prelude
  ( BlobEq (equal, equalVerify),
    swap,
  )
import Alba.Dsl.V1.Common.Contract (Contract (..))
import DslDemo.EllipticCurve.G qualified as EC
import DslDemo.EllipticCurve.JacobianWNaf qualified as EC
import DslDemo.EllipticCurve.Point (TPoint)
import DslDemo.EllipticCurve.Point qualified as EC
import Prelude ()

type EllipticCurve =
  Contract "EllipticCurve" (Base :> TNat :> TBytes) '["withdraw"] Base

contract :: EllipticCurve
contract = MkContract withdraw

-- Verifies and drops the filler. Then verifies that the provided n when
-- multiplied by G gives our desired point P. Note that this is not signing.
-- This is just a demonstration of EC scalar multiplication in script.
--
-- >>> import Alba.Dsl.V1.Bch2026 qualified as Dsl
-- >>> Dsl.progSize withdraw
-- "17 opcodes, 85 bytes. Total (with fn-table): 200 opcodes, 3143 bytes.\n"
withdraw :: CFn (Base :> TNat :> TBytes)
withdraw =
  runEnv
    ( begin
        . (verifyFiller . tabG . swap . EC.ecMul . target)
        . equal
    )
  where
    verifyFiller = opBin2Num . int 0 . equalVerify

    tabG = constant (EC.g . EC.setupTable)

    -- n = 4_000_000_000
    target :: Fn s (s :> TPoint)
    target =
      EC.pushPoint
        0xE1B5462B593D75140BEA6DD591147254FDE4AF8FE0D6AAAA987FB766BD3E2C2
        0xAFB53CBAFFB7C036E0DD9CFE799D91F3F3213E662C152E548FAC6AD36DEF1759
