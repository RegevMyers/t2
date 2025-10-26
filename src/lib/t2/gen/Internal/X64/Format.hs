module Internal.X64.Format (
    instruction,
    label
) where

import Data.Text (Text)
import qualified Data.Text as Text (length)
import Utils.List (mapInit)

instruction :: Text -> [Text] -> Text
instruction = format 1

label :: Text -> Text
label text = format 0 text []

format :: Int -> Text -> [Text] -> Text
format tabs mnemonic operands =
    mconcat [indent, align 2 mnemonic, formatOperands operands, "\n"]
    where
        formatOperands = mconcat . mapInit (align 1 . (<> ","))
        indent = tabs `times` tab
        align tabs text = text <> (tabs * 8 - Text.length text) `times` space
        tab = 8 `times` space
        space = " "

times :: Int -> Text -> Text
times n = mconcat . replicate n
