-- Copyright (c) 2026 albaDsl

module Contract (PermutationChallenge, contract) where

import Alba.Dsl.V1.Bch2026
  ( Base,
    CFN,
    FN,
    FNC,
    LibData (..),
    TBytes,
    THash256,
    begin,
    bytes,
    cast,
    int,
    nat,
    opBin2Num,
    opEqual,
    opNumEqualVerify,
    (#),
    type (>),
  )
import Alba.Dsl.V1.Bch2026.Contract.ExternalLib (importLibrary')
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Dc qualified as Dc
import Alba.Dsl.V1.Bch2026.Contract.ExternalLibs.Vc qualified as Vc
import Alba.Dsl.V1.Bch2026.Contract.Int8 (TInt8)
import Alba.Dsl.V1.Bch2026.Contract.Vector (TVector)
import Alba.Dsl.V1.Common.Contract (Contract (..))
import Numeric.Natural (Natural)
import Prelude hiding (drop)

type PermutationChallenge =
  Contract
    "PermutationChallenge"
    (Base > TBytes > TBytes)
    '["withdraw"]
    Base

contract :: PermutationChallenge
contract = MkContract withdraw

-- Verifies and drops the filler. Loads dependencies. Executes contract
-- rules.
-- >>> import Alba.Dsl.V1.Bch2026 qualified as Dsl
-- >>> Dsl.progSize withdraw
-- "26 opcodes, 137 bytes. Including function table: 41 opcodes, 201 bytes.\n"
withdraw :: CFN (Base > TBytes > TBytes)
withdraw =
  begin
    # (opBin2Num # int 0 # opNumEqualVerify)
    # loadDecompressionLib 0
    # loadVectorLib dcNumUtxos
    # (Dc.decompress # b2v # Vc.sort # target # opEqual)
  where
    dcNumUtxos = fromIntegral Dc.numUtxos

    target :: FN s (s > TVector TInt8)
    target = bytes "     Saabeeeeeeehhhhllllorsssssssty" # b2v

    b2v :: FN (s > TBytes) (s > TVector TInt8)
    b2v = cast

loadDecompressionLib :: Natural -> FNC
loadDecompressionLib startInput =
  begin
    # (bytes [254] # nat startInput # nat numUtxos)
    # (nat size # bytes Dc.lib.hash # b2h # importLibrary')
  where
    numUtxos = fromIntegral Dc.numUtxos
    size = fromIntegral Dc.lib.deploySize

b2h :: FN (s > TBytes) (s > THash256)
b2h = cast

loadVectorLib :: Natural -> FNC
loadVectorLib startInput =
  begin
    # (bytes [255] # nat startInput # nat numUtxos)
    # (nat size # bytes Vc.lib.hash # b2h # importLibrary')
  where
    numUtxos = fromIntegral Vc.numUtxos
    size = fromIntegral Vc.lib.deploySize
