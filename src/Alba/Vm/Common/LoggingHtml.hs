-- Copyright (c) 2025 albaDsl

module Alba.Vm.Common.LoggingHtml
  ( logDataToHtml,
    verifyScriptResultToHtml,
  )
where

import Alba.Vm.Common.Logging (LogDisplayOpts (..))
import Alba.Vm.Common.LoggingHtmlAssets (css, script)
import Alba.Vm.Common.LoggingTree (Node (..), logDataToTree)
import Alba.Vm.Common.ScriptError (ScriptError)
import Alba.Vm.Common.VmState (VerifyScriptResult (..), VmLogs, VmState (..))
import Control.Monad (when)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Text.Blaze (dataAttribute)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Text.Printf (printf)
import Prelude hiding (log)

logDataToHtml :: LogDisplayOpts -> Maybe VmLogs -> Text
logDataToHtml displayOpts logData =
  TL.toStrict $ renderHtml (fileHeader (logContainer displayOpts logData))

fileHeader :: Html -> Html
fileHeader content =
  H.docTypeHtml $ do
    H.head $ do
      H.title "albaVm logs"
      H.meta ! A.charset "UTF-8"
      H.style $ H.toHtml css
    H.body content
    H.script $ H.preEscapedToMarkup script

logContainer :: LogDisplayOpts -> Maybe VmLogs -> Html
logContainer _ Nothing = H.p "No logs."
logContainer displayOpts (Just logData) = do
  let tree = logDataToTree displayOpts logData
   in H.div ! A.class_ "log-container" $ do
        H.div ! A.class_ "log-header" $ do
          H.div ""
          H.div "Operation"
          H.div "Stack"
          H.div ""
          H.div ""
        H.div ! A.id "log-entries" $ do
          mapM_ loop tree.children

loop :: Node -> Html
loop Node {..} = do
  H.div
    ! A.class_
      ( fromString
          ( printf "log-entry level-%d" colorCycle
              <> if hasChildren then " has-children" else ""
          )
      )
    ! dataAttribute "path" (fromString (pathStr path))
    $ do
      H.div ! A.class_ "triangle" $ if hasChildren then triangle else ""
      H.div ! A.class_ "col-opcode" $ H.toHtml opcode
      H.div ! A.class_ "col-stack" $ H.toHtml stack
      H.div ! A.class_ "col-alt" $ ""
      H.div ! A.class_ "col-cost" $ ""
      when hasChildren $ do
        H.div ! A.class_ "stack" ! A.hidden "" $ H.toHtml stack
        H.div ! A.class_ "stackSummary" ! A.hidden "" $
          H.toHtml (fromMaybe "" stackSummary)
  mapM_ loop children
  where
    hasChildren = not (null children)

    triangle = "▼"

    colorCycle = 1 + (length path - 2) `rem` 14

    pathStr :: [Int] -> String
    pathStr xs = intercalate "." (show <$> reverse xs)

verifyScriptResultToHtml ::
  LogDisplayOpts ->
  Either (ScriptError, VerifyScriptResult) VerifyScriptResult ->
  Text
verifyScriptResultToHtml displayOpts result = do
  let (VerifyScriptResult {..}, msg) = case result of
        (Right res) -> (res, H.text "Successful script verification.")
        (Left (scriptError, res)) ->
          let str =
                printf "Script verification failed with: %s" (show scriptError)
           in (res, H.text (T.pack str))
      content =
        section "scriptSig" scriptSigResult
          <> section "scriptPubKey" scriptPubKeyResult
          <> section "redeemScript" scriptRedeemResult
          <> H.p msg
  TL.toStrict $ renderHtml $ fileHeader content
  where
    section :: H.Markup -> Maybe VmState -> Html
    section header =
      maybe (H.p "") (\x -> H.p header <> logContainer displayOpts x.logData)
