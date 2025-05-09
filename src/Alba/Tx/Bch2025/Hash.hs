-- Copyright (c) 2025 albaDsl

module Alba.Tx.Bch2025.Hash
  ( Hash256 (..),
    Hash160 (..),
    ripemd160,
    sha1,
    sha256,
    hash160,
    hash160',
    hash256,
    hash256',
    hash256zero,
  )
where

import Alba.Vm.Common.BasicTypes (Bytes)
import Crypto.Hash (Digest, RIPEMD160, SHA1, SHA256)
import Crypto.Hash qualified as H
import Data.Binary (Binary (..))
import Data.Binary.Get (getByteString)
import Data.Binary.Put (putByteString)
import Data.ByteArray (convert)
import Data.ByteString qualified as B

newtype Hash256 = Hash256 {hash :: Bytes} deriving (Eq, Ord, Show)

instance Binary Hash256 where
  put (Hash256 hash) = putByteString hash
  get = Hash256 <$> getByteString 32

newtype Hash160 = Hash160 {hash :: Bytes} deriving (Eq, Ord, Show)

instance Binary Hash160 where
  put (Hash160 hash) = putByteString hash
  get = Hash160 <$> getByteString 20

ripemd160 :: Bytes -> Bytes
ripemd160 x = convert (H.hash x :: Digest RIPEMD160)

sha1 :: Bytes -> Bytes
sha1 x = convert (H.hash x :: Digest SHA1)

sha256 :: Bytes -> Bytes
sha256 x = convert (H.hash x :: Digest SHA256)

hash160 :: Bytes -> Bytes
hash160 x =
  let h1 = H.hash x :: Digest SHA256
      h2 = H.hash h1 :: Digest RIPEMD160
   in convert h2

hash160' :: Bytes -> Hash160
hash160' x = Hash160 $ hash160 x

hash256 :: Bytes -> Bytes
hash256 x =
  let h1 = H.hash x :: Digest SHA256
      h2 = H.hash h1 :: Digest SHA256
   in convert h2

hash256' :: Bytes -> Hash256
hash256' x = Hash256 (hash256 x)

hash256zero :: Hash256
hash256zero = Hash256 $ B.replicate hash256Length 0
  where
    hash256Length = 32
