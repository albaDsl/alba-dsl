-- Copyright (c) 2025 albaDsl

module Contract (LastWill, Params, contract) where

import Alba.Dsl.V1.Bch2025
import Data.Word (Word64)
import Numeric.Natural (Natural)
import Params (fee, inheritDelay, refreshDelay)
import Prelude hiding (drop)

type LastWill =
  Contract
    "LastWill"
    ( Base
        > (Base > N "pubKey" TPubKey > N "sig" TSig)
          :| (Base > N "pubKey" TPubKey > N "sig" TSig)
          :| (Base > N "pubKey" TPubKey > N "sig" TSig)
    )
    '["refresh", "withdraw", "inherit"]
    Params

type Params =
  ( Base
      > N "refreshHash" THash160
      > N "withdrawHash" THash160
      > N "inheritHash" THash160
  )

contract :: LastWill
contract = MkContract $ entry3 refresh withdraw inherit

refresh :: CFN (Append (Base > N "pubKey" TPubKey > N "sig" TSig) Params)
refresh =
  begin
    # ( begin
          # (roll @"sig" # roll @"refreshHash" # roll @"pubKey")
          # verifyAuthorized
      )
    # verifyOutputAmount 0
    # verifyOutputScript 0
    # verifySequence refreshDelay
    # (drop @"withdrawHash" # drop @"inheritHash")
    # opTrue
  where
    verifyOutputAmount :: Natural -> FNC
    verifyOutputAmount outputIndex =
      begin
        # (opInputIndex # opUtxoValue # subMinerFee fee)
        # (nat (fromIntegral outputIndex) # opOutputValue)
        # opEqualVerify

    subMinerFee :: Word64 -> FN (s > TNat) (s > TNat)
    subMinerFee minerFee = nat (fromIntegral minerFee) # natSub

    verifyOutputScript :: Natural -> FNC
    verifyOutputScript outputIndex =
      begin
        # (opInputIndex # opUtxoBytecode)
        # (nat (fromIntegral outputIndex) # opOutputBytecode)
        # opEqualVerify

withdraw :: CFN (Append (Base > N "pubKey" TPubKey > N "sig" TSig) Params)
withdraw =
  begin
    # ( begin
          # (roll @"sig" # roll @"withdrawHash" # roll @"pubKey")
          # verifyAuthorized
      )
    # (drop @"refreshHash" # drop @"inheritHash")
    # opTrue

inherit :: CFN (Append (Base > N "pubKey" TPubKey > N "sig" TSig) Params)
inherit =
  begin
    # ( begin
          # (roll @"sig" # roll @"inheritHash" # roll @"pubKey")
          # verifyAuthorized
      )
    # verifySequence inheritDelay
    # (drop @"refreshHash" # drop @"withdrawHash")
    # opTrue

verifyAuthorized :: FN (s > TSig > THash160 > TPubKey) s
verifyAuthorized = opDup # opHash160 # opRot # opEqualVerify # opCheckSigVerify

verifySequence :: Natural -> FNC
verifySequence t = nat (timeSequence t) # opCheckSequenceVerify # opDrop
