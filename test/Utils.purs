module Test.Utils where

import Prelude

import Data.Nullable (Nullable, notNull, null)
import PdfMake.Unsafe (Content, Style, Table(..))

defaultStyle ∷ Style
defaultStyle = 
  { fontSize: null
  , font: null
  , margin: null
  }

defaultContent ∷ Content
defaultContent = 
  { table: null
  , text: null
  , rowSpan: null
  , colSpan: null
  , style: null
  }

text ∷ String → Content
text s = defaultContent { text = notNull s }

table ∷ Array (Array Content) → Content
table = table_ null

table_ ∷ Nullable (Array String) → Array (Array Content) → Content
table_ widths body = defaultContent { table = notNull t }
  where t = Table { body: body, widths: widths }

nn = notNull

setStyle
  ∷ ∀ r
  . String
  → { style ∷ Nullable String | r }
  → { style ∷ Nullable String | r }
setStyle s = _ { style = nn $ s }
