-- Copyright (c) 2025 albaDsl
{-# LANGUAGE TemplateHaskell #-}

module Alba.Vm.Common.LoggingHtmlAssets where

import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding qualified as T

css :: Text
css = T.decodeUtf8 $(embedFile "src/Alba/Vm/Common/Html/style.css")

script :: Text
script = T.decodeUtf8 $(embedFile "src/Alba/Vm/Common/Html/script.js")
