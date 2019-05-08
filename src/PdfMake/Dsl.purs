module PdfMake.Dsl where

import Data.Array ((:))
import Data.Typelevel.Num (class Add, toInt')
import Type.Prelude (Proxy(..))

type Cell r = { content ∷ String | r }

newtype CellValue rowspan = CellValue (Cell ())

type ColumnCell = Cell (rowspan ∷ Int)

newtype Column height colspan = Column (Array ColumnCell)

type RowColumns = Array ({ colspan ∷ Int, cells ∷ Array ColumnCell })
newtype Row height width = Row RowColumns

newtype Table width = Table (Array (RowColumns))

consCell ∷
  ∀ colspan rowspan height height'
  . (Add rowspan height height')
  ⇒ CellValue rowspan
  → Column height colspan
  → Column height' colspan
consCell (CellValue cell) (Column cells) =
  let
    cell' = { content: cell.content, rowspan: toInt' (Proxy ∷ Proxy rowspan) }
  in
    Column (cell' : cells)

consColumn ∷
  ∀ colspan height width width'
  . (Add colspan width width')
  ⇒ Column height colspan
  → Row height width
  → Row height width'
consColumn (Column column) (Row columns) =
  let
    column' = { colspan: toInt' (Proxy ∷ Proxy colspan), cells: column }
  in
    Row (column' : columns)

consRow ∷
  ∀ h width
  . Row h width
  → Table width
  → Table width
consRow (Row row) (Table rows) =
  Table (row : rows)
