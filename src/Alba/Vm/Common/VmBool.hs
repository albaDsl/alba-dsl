-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.VmBool (bytesToBool, boolToBytes) where

import Alba.Vm.Common.BasicTypes (Bytes)
import Data.ByteString qualified as B

bytesToBool :: Bytes -> Bool
bytesToBool bytes | B.null bytes = False
bytesToBool bytes | (B.any (/= 0) . B.init) bytes = True
bytesToBool bytes | B.last bytes /= 0 = B.last bytes /= 0x80
bytesToBool _bytes = False

boolToBytes :: Bool -> Bytes
boolToBytes True = B.singleton 1
boolToBytes False = B.empty
