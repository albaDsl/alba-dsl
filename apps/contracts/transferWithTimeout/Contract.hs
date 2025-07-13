-- Copyright (c) 2025 albaDsl

module Contract (TransferWithTimeout, Params, contract) where

import Alba.Dsl.V1.Bch2025
import Prelude hiding (drop)

type TransferWithTimeout =
  Contract
    "TransferWithTimeout"
    (Base > (Base > N "sig" TSig) :| (Base > N "sig" TSig))
    '["recipientWithdraw", "senderWithdraw"]
    Params

type Params =
  (Base > N "senderPub" TPubKey > N "recipientPub" TPubKey > N "timeout" TNat)

contract :: TransferWithTimeout
contract = MkContract $ entry2 recipientWithdraw senderWithdraw
  where
    recipientWithdraw =
      begin
        # (roll @"sig" # roll @"recipientPub" # opCheckSigVerify)
        # (drop @"timeout" # drop @"senderPub")
        # opTrue

    senderWithdraw =
      begin
        # (roll @"sig" # roll @"senderPub" # opCheckSigVerify)
        # (roll @"timeout" # opCheckLockTimeVerify # opDrop)
        # drop @"recipientPub"
        # opTrue
