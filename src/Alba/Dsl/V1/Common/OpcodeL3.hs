-- Copyright (c) 2025 albaDsl

module Alba.Dsl.V1.Common.OpcodeL3
  ( OpcodeL3 (..),
    CodeL3,
    FunctionId (..),
  )
where

import Alba.Vm.Common.OpcodeL2 (OpcodeL2)
import Data.Sequence qualified as S

data OpcodeL3
  = Opcode OpcodeL2
  | FunctionIndexDef {fId :: FunctionId}
  | FunctionIndexRef {fId :: FunctionId}
  | FunctionBody {code :: CodeL3}
  deriving (Eq, Show)

data FunctionId
  = Standard ModuleName LineNumber ColumnNumber FunctionName
  | Named String
  | Lambda ModuleName LineNumber ColumnNumber FunctionName
  | Absolute Slot
  deriving (Eq, Ord, Show)

type ModuleName = String

type LineNumber = Int

type ColumnNumber = Int

type FunctionName = String

type CodeL3 = S.Seq OpcodeL3

type Slot = Int
