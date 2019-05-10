module PdfMake.Dsl2 where

import Prelude

import Data.Array (range, replicate, (!!), (:))
import Data.Maybe (fromJust)
import Data.Typelevel.Num (class Add, class LtEq, class Nat, D1, toInt, toInt')
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

infixr 2 addRows as |||
infixr 3 addCols as ++

newtype Cell a colSpan rowSpan = Cell { value ∷ a }

data PrimCell a
  = Empty
  | PrimCell
    { value ∷ a
    , rowSpan ∷ Int
    , colSpan ∷ Int
    }

newtype TableH a width height = TableH (Array (Array (PrimCell a)))

mkCell ∷ ∀ a colSpan rowSpan. colSpan → rowSpan → a → Cell a colSpan rowSpan
mkCell _ _ value = Cell { value }

mkEmpty ∷ ∀ a w h. Nat w ⇒ Nat h ⇒ w → h → TableH a w h
mkEmpty w h = TableH $ replicate (toInt h) row
  where row = replicate (toInt w) Empty

fromCell ∷
  ∀ a w h
  . Nat w
  ⇒ Nat h
  ⇒ LtEq D1 w
  ⇒ LtEq D1 h
  ⇒ Cell a w h
  → TableH a w h
fromCell (Cell { value }) = TableH rows
  where
    w' = toInt' (Proxy ∷ Proxy w)
    h' = toInt' (Proxy ∷ Proxy h)
    cell = PrimCell { value, colSpan: w', rowSpan: h' }
    fstRow = cell : replicate (w' - 1) Empty
    emptyRow = replicate w' Empty
    rows = fstRow : replicate (h' - 1) emptyRow

addRows ∷
  ∀ a h1 h2 w h
  . Add h1 h2 h
  ⇒ TableH a w h1
  → TableH a w h2
  → TableH a w h
addRows (TableH t1) (TableH t2) = TableH $ t1 <> t2

addCols ∷ 
  ∀ a w1 w2 w h
  . Nat h
  ⇒ Add w1 w2 w
  ⇒ LtEq D1 h
  ⇒ TableH a w1 h
  → TableH a w2 h
  → TableH a w h
addCols (TableH t1) (TableH t2) = TableH do
  let h' = toInt' (Proxy ∷ Proxy h)
  i ← range 0 (h' - 1)
  let row1 = unsafePartial $ fromJust $ t1 !! i 
  let row2 = unsafePartial $ fromJust $ t2 !! i
  pure $ row1 <> row2

toTable ∷
  ∀ a width height
  . TableH a width height
  → Table a width
toTable (TableH t) = Table t

addTables ∷
  ∀ a width
  . Table a width
  → Table a width
  → Table a width
addTables (Table t1) (Table t2) = Table $ t1 <> t2

newtype Table a width = Table (Array (Array (PrimCell a)))

instance tableSemigroup ∷ Semigroup (Table a w) where
  append = addTables

instance tableMonoid ∷ Monoid (Table a w) where
  mempty = Table []
