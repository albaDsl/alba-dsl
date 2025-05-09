-- Copyright (c) 2025 albaDsl

module Alba.Misc.KeyPair (KeyPair (..)) where

import Crypto.Secp256k1 (PubKey, SecKey)

data KeyPair = KeyPair
  { secKey :: SecKey,
    pubKey :: PubKey
  }
  deriving (Eq, Show)
