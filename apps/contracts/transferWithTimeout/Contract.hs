-- Copyright (c) 2025 albaDsl

module Contract (TransferWithTimeout, Params, contract) where

import Alba.Dsl.V1.Bch2025

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
        # (argRoll @"sig" # argRoll @"recipientPub" # opCheckSigVerify)
        # argsDrop @2
        # opTrue

    senderWithdraw =
      begin
        # (argRoll @"sig" # argRoll @"senderPub" # opCheckSigVerify)
        # (argRoll @"timeout" # opCheckLockTimeVerify # opDrop)
        # argsDrop @1
        # opTrue
