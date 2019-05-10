module PdfMake.Dsl2 where

import Prelude

import Data.Array (range, replicate, (!!), (:))
import Data.Maybe (fromJust)
import Data.Typelevel.Num (class Add, class LtEq, class Nat, D1, toInt')
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

newtype Cell colspan rowspan = Cell { value ∷ String }

newtype PrimCell = PrimCell
  { value ∷ String
  , rowspan ∷ Int
  , colspan ∷ Int
  }

newtype TableH width height = TableH (Array (Array PrimCell))

fromCell ∷
  ∀ w h
  . Nat w
  ⇒ Nat h
  ⇒ LtEq D1 w
  ⇒ LtEq D1 h
  ⇒ Cell w h
  → TableH w h
fromCell (Cell { value }) = TableH rows
  where
    w' = toInt' (Proxy ∷ Proxy w)
    h' = toInt' (Proxy ∷ Proxy h)
    cell = PrimCell { value, colspan: w', rowspan: h' }
    empty = PrimCell { value: "", colspan: 1, rowspan: 1 }
    fstRow = cell : replicate (w' - 1) empty
    emptyRow = replicate w' empty
    rows = fstRow : replicate (h' - 1) emptyRow

addRows ∷
  ∀ h1 h2 w h
  . Add h1 h2 h
  ⇒ TableH w h1
  → TableH w h2
  → TableH w h
addRows (TableH t1) (TableH t2) = TableH $ t1 <> t2

addCols ∷ 
  ∀ w1 w2 w h
  . Nat h
  ⇒ Add w1 w2 w
  ⇒ LtEq D1 h
  ⇒ TableH w1 h
  → TableH w2 h
  → TableH w h
addCols (TableH t1) (TableH t2) = TableH do
  let h' = toInt' (Proxy ∷ Proxy h)
  i ← range 0 (h' - 1)
  let row1 = unsafePartial $ fromJust $ t1 !! i 
  let row2 = unsafePartial $ fromJust $ t2 !! i
  pure $ row1 <> row2

toTable ∷
  ∀ width height
  . TableH width height
  → Table width
toTable (TableH t) = Table t

addTables ∷
  ∀ width
  . Table width
  → Table width
  → Table width
addTables (Table t1) (Table t2) = Table $ t1 <> t2

newtype Table width = Table (Array (Array PrimCell))
