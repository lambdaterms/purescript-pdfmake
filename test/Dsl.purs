module Test.Dsl where

import Prelude

import Data.Foldable (fold)
import Data.Nullable (null, toNullable)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D1, D2, D3, D8, d1, d2, d25, d3, d5, d50)
import Effect (Effect)
import Effect.Aff (launchAff_)
import PdfMake.Dsl (Cell(..), Layout(..), PrimCell(..), PrimTable(..), TableBody(..), TableBodyH(..), addCols, addRows, fromCell, mkCell, mkEmpty, mkTable, mkTable_, showLayout, toTableBody, (++), (|||))
import PdfMake.Unsafe (Content, DocDefinition, Table(..), createPdf, defaultContent, defaultStyle)
import Test.Utils (nn, text)

toContent ∷ PrimCell Content → Content
toContent = case _ of
  Empty → defaultContent
  PrimCell { value, rowSpan, colSpan } → 
    let rs = if rowSpan == 1 then null else nn rowSpan in
    let cs = if colSpan == 1 then null else nn colSpan in
    value { rowSpan = rs, colSpan = cs }

mkCell' ∷ ∀ a. a → Cell a D1 D1
mkCell' = mkCell d1 d1

mk ∷ ∀ a. a → TableBodyH a D1 D1
mk = fromCell <<< mkCell'

txt ∷ String → TableBodyH Content D1 D1
txt = mk <<< text

hd ∷ String → TableBodyH Content D1 D1
hd = mk <<< styleTableHeader <<< text

logoPart ∷ TableBodyH _ D3 D3
logoPart = logoElem ++ rightHeader
  where
    logoElem ∷ TableBodyH _ D1 D3
    logoElem = fromCell $ mkCell d1 d3 $ text "logo"

    rightHeader ∷ TableBodyH _ D2 D3
    rightHeader = 
      txt "prof" ++ txt "emp" |||
      txt "issueDate" ++ txt "dueDate" |||
      txt "issueDate_" ++ txt "dueDate_"

listPart ∷ TableBody _ D8
listPart = header <> list <> footer
  where
    elem (no /\ name /\ unit /\ qty /\ price /\ ammount /\ vat /\ gross) = toTableBody $
      txt no ++ txt name ++ txt unit ++ txt qty ++ txt price ++ txt ammount ++ txt vat ++ txt gross
    list = fold $ map elem
      [ "1" /\ "VIDEO STREAMS [montly subscription]\nstart:2019-03-15; host:stream4.nadaje.com:8580; max recipients:50" 
        /\ "service" /\ "1" /\ "zl123.32" /\ "zl123.32" /\ "23%" /\ "zl123.42"
      , "2" /\ "VIDEO STREAMS [montly subscription]\nstart:2019-06-15; host:stream4.nadaje.com:8580; max recipients:70" 
        /\ "service" /\ "3" /\ "zl155.32" /\ "zl123.32" /\ "23%" /\ "zl123.42"
      ]
    footer = toTableBody $ mkEmpty d5 d2 ++ 
      ( hd "Net Amount" ++ hd "VAT" ++ hd "Gross"
      ||| txt "zl1323.32" ++ txt "23%" ++ txt "zl4421.42"
      )
    header = toTableBody $ hd "No." ++ hd "Name" ++ hd "Unit" ++ hd "Qty"
      ++ hd "Net price" ++ hd "Net Amount" ++ hd "VAT rate" ++ hd "Gross"

tableToContent ∷ PrimTable Content → Content
tableToContent (PrimTable { body, layout, widths }) =
  defaultContent { table = nn t, layout = nn $ showLayout layout }
    where
      ws = toNullable $ map (\n → show n <> "%") <$> widths
      t = Table { body: map (map toContent) body, widths: ws }

styleTableHeader ∷ Content → Content
styleTableHeader = _
  { bold = nn true
  , color = nn "black"
  }

dd ∷ DocDefinition
dd = 
  let dc = defaultContent in
  { content: 
      [ text "dd dsl"
      , styleTable $ tableToContent $ 
        mkTable LightHorizontalLines (d50 /\ d25 /\ d25) $ toTableBody logoPart
      , styleTable $ tableToContent $ mkTable_ LightHorizontalLines $ listPart
      ]
  , defaultStyle: nn $ defaultStyle 
      { font = nn "Times"
      , fontSize = nn 10
      }
  }
  where
    
    styleTable = _
      { margin = nn [0, 5, 0, 15]
      , layout = nn "lightHorizontalLines"
      , color = nn "#444"
      }

main ∷ Effect Unit
main = launchAff_ do
  createPdf dd "etc/dsl-dd.pdf"
