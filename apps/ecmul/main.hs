import Alba.Dsl.V1.Bch2026
import Alba.Vm.Bch2026
import Alba.Vm.Common.VmLimits (dumpMetrics)
import Data.Maybe (fromJust)
import DslDemo.EllipticCurve.EllipticCurve (ecMul)
import DslDemo.EllipticCurve.EllipticCurveConstants (g)
import DslDemo.EllipticCurve.EllipticCurvePoint (getX)
import Numeric.Natural (Natural)

-- For profiling the VM using the EC multiply example.
main :: IO ()
main = do
  let n = 115792089237316195423570985008687907852837564279074904382605163141518161494336
      c = compile O1 (progMul n)
  ecMultiply c

ecMultiply :: CodeL1 -> IO ()
ecMultiply code =
  case vmEval code of
    Right state -> do
      print state.s
      dumpMetrics state
    Left err -> error ("err: " <> show err)

progMul :: Natural -> FN s (s > TInt)
progMul scalar = nat scalar # g # ecMul # getX

vmEval :: CodeL1 -> Either ScriptError VmState
vmEval code =
  let state = (startState vmParamsStandard) {code, logData = Nothing}
   in case evaluateScript context state of
        Right state' -> Right state'
        Left (err, _) -> Left err
  where
    context = fromJust $ mkTxContext undefined 0 undefined
