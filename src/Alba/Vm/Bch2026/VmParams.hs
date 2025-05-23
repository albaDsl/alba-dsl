-- Copyright (c) 2025 albaDsl

module Alba.Vm.Bch2026.VmParams
  ( vmParamsStandard,
    vmParamsNonStandard,
  )
where

import Alba.Vm.Common.VmInteger (maxInteger)
import Alba.Vm.Common.VmParams (HugeInt (..), SigLimit (..), VmParams (..))
import Data.Bits (shift)

vmParamsStandard :: VmParams
vmParamsStandard =
  let maxScriptElementSize = 10_000
   in VmParams
        { integerMin = HugeInt (maxInteger maxScriptElementSize True),
          integerMax = HugeInt (maxInteger maxScriptElementSize False),
          maxIntegerBytes = maxScriptElementSize,
          maxScriptSize = maxScriptElementSize,
          maxScriptElementSize = maxScriptElementSize,
          maxStackSize = 1000,
          maxExecStackSize = 100,
          lockTimeThreshold = 500_000_000,
          opCodeCost = 100,
          costBudgetPerInputByte = 800,
          hashDigestIterationCost = 192,
          sigCheckCost = 26_000,
          fixedCredit = 41,
          hashItersLimitNumerator = 1,
          hashItersLimitDenominator = 2,
          sigLimit = DclBased {fixedCredit = 60, denominator = 43},
          sequenceFinal = 0xffff_ffff,
          sequenceLocktimeDisableFlag = 1 `shift` 31,
          sequenceLocktimeTypeFlag = 1 `shift` 22,
          sequenceLocktimeMask = 0x0000ffff,
          discourageNops = True,
          minTxSize = 65,
          maxStandardTxSize = 100000,
          maxTxInScriptSigSize = Nothing,
          maxCommitmentSize = 128
        }

vmParamsNonStandard :: VmParams
vmParamsNonStandard =
  vmParamsStandard
    { hashItersLimitNumerator = 7,
      hashDigestIterationCost = 64,
      sigLimit = MaxLimit {limit = 3000},
      discourageNops = False
    }
