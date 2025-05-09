-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2025.SigEncoding
  ( checkDataSignatureEncoding,
    checkRawEcdsaSignatureEncoding,
    checkTransactionSchnorrSignatureEncoding,
    checkPubKeyEncoding,
  )
where

import Alba.Misc.Utils (canNotHappen)
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.StackElement (Bytes)
import Data.ByteString qualified as B

compressedPublicKeySize :: Int
compressedPublicKeySize = 33

publicKeySize :: Int
publicKeySize = 65

checkDataSignatureEncoding :: Bytes -> Either ScriptError ()
checkDataSignatureEncoding sig =
  if B.null sig
    then Right ()
    else checkRawSignatureEncoding sig

checkRawSignatureEncoding :: Bytes -> Either ScriptError ()
checkRawSignatureEncoding sig =
  if isSchnorrSig sig
    then Right ()
    else checkRawEcdsaSignatureEncoding sig

isSchnorrSig :: Bytes -> Bool
isSchnorrSig sig = B.length sig == 64

-- FIXME: Implement remaining checks.
checkRawEcdsaSignatureEncoding :: Bytes -> Either ScriptError ()
checkRawEcdsaSignatureEncoding sig =
  if isSchnorrSig sig
    then Left SeSigBadLength
    else Right ()

checkTransactionSchnorrSignatureEncoding :: Bytes -> Either ScriptError ()
checkTransactionSchnorrSignatureEncoding sig | B.null sig = Right ()
checkTransactionSchnorrSignatureEncoding sig
  | isSchnorrSig (B.dropEnd 1 sig) = Right ()
checkTransactionSchnorrSignatureEncoding _ = Left SeSigNonSchnorr

checkPubKeyEncoding :: Bytes -> Either ScriptError ()
checkPubKeyEncoding pubKey =
  if isCompressedOrUncompressedPubKey pubKey
    then Right ()
    else Left SePubKeyType

isCompressedOrUncompressedPubKey :: Bytes -> Bool
isCompressedOrUncompressedPubKey pubKey =
  case B.length pubKey of
    x | x == compressedPublicKeySize ->
      case B.indexMaybe pubKey 0 of
        Just x' -> x' == 0x02 || x' == 0x03
        Nothing -> canNotHappen
    x | x == publicKeySize ->
      case B.indexMaybe pubKey 0 of
        Just x' -> x' == 0x04
        Nothing -> canNotHappen
    _ -> False
