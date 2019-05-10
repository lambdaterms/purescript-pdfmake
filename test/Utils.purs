module Test.Utils where

import Prelude

import Data.Nullable (Nullable, notNull, null)
import PdfMake.Unsafe (Content, Style, Table(..), defaultContent)

text ∷ String → Content
text s = defaultContent { text = notNull s }

table ∷ Array (Array Content) → Content
table = table_ null

table_ ∷ Nullable (Array String) → Array (Array Content) → Content
table_ widths body = defaultContent { table = notNull t }
  where t = Table { body: body, widths: widths }

nn = notNull
