-- Copyright (c) 2026 albaDsl

module Contract (PermutationChallenge, contract) where

import Alba.Dsl.V1.Bch2026
  ( CFn,
    Fn,
    FnC,
    LibData (..),
    Stack (..),
    TBytes,
    THash256,
    begin,
    bytes,
    cast,
    functionId,
    int,
    nat,
    opBin2Num,
    reserveSlots,
    (.),
  )
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Dc qualified as Dc
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Vc qualified as Vc
import Alba.Dsl.V1.Bch2026.Contract.Prelude (BlobEq (..), TInt8, importLibrary')
import Alba.Dsl.V1.Bch2026.Contract.TVector (TVector)
import Alba.Dsl.V1.Common.Contract (Contract (..))
import Numeric.Natural (Natural)
import Prelude (Integral, fromIntegral)

type PermutationChallenge =
  Contract
    "PermutationChallenge"
    (Base :> TBytes :> TBytes)
    '["withdraw"]
    Base

contract :: PermutationChallenge
contract = MkContract withdraw

-- Verifies and drops the filler. Loads dependencies. Executes contract
-- rules.
-- >>> import Alba.Dsl.V1.Bch2026 qualified as Dsl
-- >>> Dsl.progSize withdraw
-- "26 opcodes, 136 bytes. Including function table: 41 opcodes, 200 bytes.\n"
withdraw :: CFn (Base :> TBytes :> TBytes)
withdraw =
  begin
    . (opBin2Num . int 0 . equalVerify)
    . loadDecompressionLib 0
    . loadVectorLib dcNumUtxos
    . (Dc.decompress . b2v . Vc.sort . target . equal)
  where
    dcNumUtxos = fromIntegral Dc.numUtxos

    target :: Fn s (s :> TVector TInt8)
    target = bytes "     Saabeeeeeeehhhhllllorsssssssty" . b2v

    b2v :: Fn (s :> TBytes) (s :> TVector TInt8)
    b2v = cast

loadDecompressionLib :: Natural -> FnC
loadDecompressionLib startInput =
  begin
    . reserveSlots [slot]
    . (functionId slot . nat startInput . nat numUtxos)
    . (nat size . bytes Dc.lib.hash . b2h . importLibrary')
  where
    slot :: (Integral a) => a
    slot = 254

    numUtxos = fromIntegral Dc.numUtxos
    size = fromIntegral Dc.lib.deploySize

b2h :: Fn (s :> TBytes) (s :> THash256)
b2h = cast

loadVectorLib :: Natural -> FnC
loadVectorLib startInput =
  begin
    . reserveSlots [slot]
    . (functionId slot . nat startInput . nat numUtxos)
    . (nat size . bytes Vc.lib.hash . b2h . importLibrary')
  where
    slot :: (Integral a) => a
    slot = 255

    numUtxos = fromIntegral Vc.numUtxos
    size = fromIntegral Vc.lib.deploySize
