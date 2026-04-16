-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.OpcodeL3
  ( OpcodeL3 (..),
    CodeL3,
    FunctionId (..),
    FunctionIdType (..),
    VmFunctionId,
    mkVmFunctionId,
    vmFunctionIdToByteString,
    isConstant,
    isRtConstant,
  )
where

import Alba.Vm.Common.OpcodeL2 (OpcodeL2)
import Alba.Vm.Common.VmInteger (integerToBytesUnsigned)
import Data.ByteString qualified as B
import Data.Char (ord)
import Data.Sequence qualified as S
import Data.String (IsString (..))
import Data.Word (Word8)

data OpcodeL3
  = Opcode OpcodeL2
  | FunctionIndexDef {fId :: FunctionId}
  | FunctionIndexRef {fId :: FunctionId}
  | FunctionBody {code :: CodeL3}
  | RuntimeState
  deriving (Eq, Show)

data FunctionId
  = Standard ModuleName LineNumber ColumnNumber FunctionName
  | Constant ModuleName LineNumber ColumnNumber FunctionName
  | RuntimeConstant ModuleName LineNumber ColumnNumber FunctionName
  | Lambda ModuleName LineNumber ColumnNumber FunctionName
  | Named String
  | Absolute Index
  deriving (Eq, Ord, Show)

type ModuleName = String

type LineNumber = Int

type ColumnNumber = Int

type FunctionName = String

type CodeL3 = S.Seq OpcodeL3

type Index = Int

data FunctionIdType = Local | ThreeByte16_8 VmFunctionId

newtype VmFunctionId = VmFunctionId [Word8]
  deriving (Eq)

instance Show VmFunctionId where
  show (VmFunctionId x) = show x

instance Semigroup VmFunctionId where
  (VmFunctionId x) <> (VmFunctionId y) = VmFunctionId (x <> y)

instance Monoid VmFunctionId where
  mempty = VmFunctionId []
  mappend = (<>)

instance IsString VmFunctionId where
  fromString str = VmFunctionId $ map (fromIntegral . ord) str

vmFunctionIdToByteString :: VmFunctionId -> B.ByteString
vmFunctionIdToByteString (VmFunctionId x) = B.pack x

-- 'Local': We consider 0-byte, 1-byte, and 2-byte identifiers as part of the
-- local Function Identifier space for the contract.
--
-- 'ThreeByte16_8': Meant for external libraries that should not conflict with
-- the local function space. These are three byte identifiers where two bytes
-- (16-bits) are used as the library prefix.
mkVmFunctionId :: FunctionIdType -> Int -> VmFunctionId
mkVmFunctionId Local x
  | x < 2 ^ (16 :: Int) =
      let bytes = integerToBytesUnsigned (fromIntegral x)
       in VmFunctionId (B.unpack bytes)
  | otherwise = errIndexLimit
mkVmFunctionId (ThreeByte16_8 prefix) x
  | x == 0 && idLength prefix == 2 = (prefix <> VmFunctionId [0])
  | x < 256 && idLength prefix == 2 =
      let bytes = integerToBytesUnsigned (fromIntegral x)
       in (prefix <> VmFunctionId (B.unpack bytes))
  | otherwise = errIndexLimit
  where
    idLength (VmFunctionId str) = length str

errIndexLimit :: a
errIndexLimit =
  error "mkVmFunctionId: function index limit exceeded or prefix out of range."

isConstant :: FunctionId -> Bool
isConstant (Constant _ _ _ _) = True
isConstant _ = False

isRtConstant :: FunctionId -> Bool
isRtConstant (RuntimeConstant _ _ _ _) = True
isRtConstant _ = False
