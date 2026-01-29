-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.OpcodeL3
  ( OpcodeL3 (..),
    CodeL3,
    FunctionId (..),
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
  deriving (Eq, Show)

data FunctionId
  = Standard ModuleName LineNumber ColumnNumber FunctionName
  | Constant ModuleName LineNumber ColumnNumber FunctionName
  | RuntimeConstant ModuleName LineNumber ColumnNumber FunctionName
  | Lambda ModuleName LineNumber ColumnNumber FunctionName
  | Named String
  | Absolute Index
  deriving (Eq, Ord, Show)

newtype VmFunctionId = VmFunctionId [Word8]
  deriving (Eq)

type ModuleName = String

type LineNumber = Int

type ColumnNumber = Int

type FunctionName = String

type CodeL3 = S.Seq OpcodeL3

type Index = Int

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

-- If prefix is empty then use [] for index zero. Otherwise, use [0] for index
-- zero. Rationale: if we don't have a prefix we want to use [] since it is
-- cheap to push (using op0). When we have a prefix, we want the identifiers to
-- stay the same length. For contract local identifiers, which are unprefixed
-- and 0-2 bytes in size, certain identifiers don't get used, e.g.: [0] and
-- [*, 0].
-- FIXME: add padding support for prefixed identifiers.
mkVmFunctionId :: VmFunctionId -> Int -> VmFunctionId
mkVmFunctionId (VmFunctionId []) 0 = VmFunctionId []
mkVmFunctionId prefix 0 = prefix <> VmFunctionId [0]
mkVmFunctionId prefix x =
  let bytes = integerToBytesUnsigned (fromIntegral x)
   in prefix <> VmFunctionId (B.unpack bytes)

isConstant :: FunctionId -> Bool
isConstant (Constant _ _ _ _) = True
isConstant _ = False

isRtConstant :: FunctionId -> Bool
isRtConstant (RuntimeConstant _ _ _ _) = True
isRtConstant _ = False
