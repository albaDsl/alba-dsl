-- Copyright (c) 2025 albaDsl

module Contract (TransferWithTimeout, Params, contract) where

import Alba.Dsl.V1.Bch2026
  ( Contract (..),
    N,
    Stack (..),
    TNat,
    TPubKey,
    TSig,
    begin,
    del,
    entry2,
    opCheckLockTimeVerify,
    opCheckSigVerify,
    opDrop,
    opTrue,
    roll,
    (.),
    type (:|),
  )
import Prelude hiding ((.))

type TransferWithTimeout =
  Contract
    "TransferWithTimeout"
    (Base :> (Base :> N "sig" TSig) :| (Base :> N "sig" TSig))
    '["recipientWithdraw", "senderWithdraw"]
    Params

type Params =
  ( Base
      :> N "senderPub" TPubKey
      :> N "recipientPub" TPubKey
      :> N "timeout" TNat
  )

contract :: TransferWithTimeout
contract = MkContract $ entry2 recipientWithdraw senderWithdraw
  where
    recipientWithdraw =
      begin
        . (roll #sig . roll #recipientPub . opCheckSigVerify)
        . (del #timeout . del #senderPub)
        . opTrue

    senderWithdraw =
      begin
        . (roll #sig . roll #senderPub . opCheckSigVerify)
        . (roll #timeout . opCheckLockTimeVerify . opDrop)
        . del #recipientPub
        . opTrue
