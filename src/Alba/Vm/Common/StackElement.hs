-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.StackElement
  ( StackElement (..),
    Bytes,
    Labels,
    integerToStackElement,
    bytesToStackElement,
    boolToStackElement,
    stackElementToInteger,
    stackElementToInteger',
    stackElementToBytes,
    stackElementToBool,
    stackElementToBool',
    showStackElement,
    verifyStackElementSize,
    verifyIntegerByteSizeInRange,
    verifyIntegerInRange,
    i2SeUnsafe,
    b2SeUnsafe,
  )
where

import Alba.Vm.Common.BasicTypes (Bytes)
import Alba.Vm.Common.ScriptError (ScriptError (..))
import Alba.Vm.Common.Utils (Labels, formatBytes, formatBytesWithLabels)
import Alba.Vm.Common.VmBool (boolToBytes, bytesToBool)
import Alba.Vm.Common.VmInteger
  ( bytesToInteger,
    integerByteSize,
    integerToBytes,
  )
import Alba.Vm.Common.VmParams (HugeInt (..), VmParams (..))
import Control.Exception (assert)
import Control.Monad (unless)
import Data.Bits ((.&.))
import Data.ByteString qualified as B
import Data.Text qualified as T

data StackElement = StackElement
  { value :: !StackElementValue,
    byteSize :: !Int
  }
  deriving (Show)

instance Eq StackElement where
  x == y = stackElementToBytes x == stackElementToBytes y

data StackElementValue = SevInteger Integer | SevBytes Bytes | SevBool Bool

instance Show StackElementValue where
  show x =
    case x of
      SevInteger x' -> show x'
      SevBytes x' -> T.unpack $ formatBytes x'
      SevBool x' -> show x'

-- Alternative to the Show instance that supports labels and does not show the
-- byteSize.
showStackElement :: Maybe Labels -> StackElement -> T.Text
showStackElement labels se =
  case se.value of
    SevInteger x -> T.pack $ show x
    SevBytes x -> formatBytesWithLabels labels x
    SevBool x -> T.pack $ show x

integerToStackElement :: VmParams -> Integer -> Either ScriptError StackElement
integerToStackElement vmParams x = do
  verifyIntegerInRange vmParams x
  Right $ StackElement (SevInteger x) (integerByteSize x)

bytesToStackElement :: VmParams -> Bytes -> Either ScriptError StackElement
bytesToStackElement vmParams x = do
  verifyByteSizeInRange vmParams x
  Right $ StackElement (SevBytes x) (B.length x)

boolToStackElement :: Bool -> StackElement
boolToStackElement True = StackElement (SevBool True) 1
boolToStackElement False = StackElement (SevBool False) 0

stackElementToInteger :: VmParams -> StackElement -> Either ScriptError Integer
stackElementToInteger vmParams =
  stackElementToInteger' True vmParams.maxIntegerBytes vmParams

stackElementToInteger' ::
  Bool -> Int -> VmParams -> StackElement -> Either ScriptError Integer
stackElementToInteger' _ maxSize _ (StackElement (SevInteger x) byteSize) = do
  verifyIntegerByteSizeInRange' byteSize maxSize
  pure x
stackElementToInteger'
  check
  maxSize
  vmParams
  (StackElement (SevBytes x) byteSize) = do
    verifyIntegerByteSizeInRange' byteSize maxSize
    if check then verifyMinimallyEncoded vmParams x else pure ()
    pure $ bytesToInteger x
stackElementToInteger' _ maxSize _ (StackElement (SevBool True) _) =
  assert (maxSize >= 1) $ Right 1
stackElementToInteger' _ _ _ (StackElement (SevBool False) _) = Right 0

stackElementToBytes :: StackElement -> Bytes
stackElementToBytes (StackElement (SevInteger x) _) = integerToBytes x
stackElementToBytes (StackElement (SevBytes x) _) = x
stackElementToBytes (StackElement (SevBool x) _) = boolToBytes x

stackElementToBool :: VmParams -> StackElement -> Either ScriptError Bool
stackElementToBool _ (StackElement (SevInteger x) _) = pure $ x /= 0
stackElementToBool vmParams se@(StackElement (SevBytes _) _) = do
  x <- stackElementToInteger vmParams se
  pure $ x /= 0
stackElementToBool _ (StackElement (SevBool x) _) = pure x

stackElementToBool' :: StackElement -> Bool
stackElementToBool' (StackElement (SevInteger x) _) = x /= 0
stackElementToBool' (StackElement (SevBytes x) _) = bytesToBool x
stackElementToBool' (StackElement (SevBool x) _) = x

verifyByteSizeInRange :: VmParams -> Bytes -> Either ScriptError ()
verifyByteSizeInRange vmParams x = verifyStackElementSize vmParams (B.length x)

verifyStackElementSize :: VmParams -> Int -> Either ScriptError ()
verifyStackElementSize vmParams x =
  unless (x <= vmParams.maxScriptElementSize) $ Left SePushSize

verifyIntegerByteSizeInRange :: VmParams -> Bytes -> Either ScriptError ()
verifyIntegerByteSizeInRange vmParams x =
  unless (B.length x <= vmParams.maxIntegerBytes) $ Left SeInvalidNumberRange

verifyIntegerByteSizeInRange' :: Int -> Int -> Either ScriptError ()
verifyIntegerByteSizeInRange' size maxSize =
  unless (size <= maxSize) $ Left SeInvalidNumberRange

verifyIntegerInRange :: VmParams -> Integer -> Either ScriptError ()
verifyIntegerInRange vmParams x =
  unless (x >= vmParams.integerMin.val && x <= vmParams.integerMax.val) $
    Left SeInvalidNumberRange

verifyMinimallyEncoded :: VmParams -> Bytes -> Either ScriptError ()
verifyMinimallyEncoded vmParams bytes =
  unless (isMinimallyEncoded vmParams bytes) $ Left SeMinimalNum

-- Only used by testing code to prepare stacks.
i2SeUnsafe :: Integer -> StackElement
i2SeUnsafe x = StackElement (SevInteger x) (integerByteSize x)

b2SeUnsafe :: Bytes -> StackElement
b2SeUnsafe x = StackElement (SevBytes x) (B.length x)

isMinimallyEncoded :: VmParams -> Bytes -> Bool
isMinimallyEncoded _ bytes | B.null bytes = True
isMinimallyEncoded vmParams bytes
  | B.length bytes > vmParams.maxScriptElementSize = False
isMinimallyEncoded _ bytes =
  not
    ( (B.last bytes .&. 0x7f == 0)
        && (B.length bytes == 1 || (B.last . B.init) bytes .&. 0x80 == 0)
    )
