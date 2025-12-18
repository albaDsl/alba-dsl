-- Copyright (c) 2025 albaDsl

module Alba.Misc.Logging (dumpLogToFile) where

import Alba.Dsl.V1.Bch2025 (CompilationResult (..), FunctionTable)
import Alba.Dsl.V1.Common.FunctionTableJson
  ( FunctionTableEntry (..),
    tableEntries,
  )
import Alba.Vm.Bch2025 qualified as Bch2025 ()
import Alba.Vm.Bch2026 qualified as Bch2026
import Alba.Vm.Common.Logging (defaultDisplayOpts)
import Alba.Vm.Common.Logging qualified as Log
import Alba.Vm.Common.LoggingHtml (logDataToHtml)
import Alba.Vm.Common.VmState (VmLogs)
import Data.Map qualified as M
import Data.Text.IO qualified as T

dumpLogToFile :: Maybe CompilationResult -> Maybe VmLogs -> FilePath -> IO ()
dumpLogToFile cr logs file = do
  let opts =
        maybe
          defaultDisplayOpts
          ( \CompilationResult {..} ->
              defaultDisplayOpts
                { Log.functionTable = Just $ convertTable functionTable
                }
          )
          cr
      html = logDataToHtml opts logs
  T.writeFile file html

convertTable :: FunctionTable -> Log.FunctionTable
convertTable functions =
  let entries = tableEntries functions
      entries' = (\e -> (e.slot, convert e)) <$> entries
   in M.fromList entries'

convert :: FunctionTableEntry -> Log.FunctionTableEntry
convert FunctionTableEntry {..} = Log.FunctionTableEntry {..}
