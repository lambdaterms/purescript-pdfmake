module PdfMake.Dsl where

import Prelude

import Data.Array (range, replicate, (!!), (:))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (class Add, class LtEq, class Nat, class Succ, D1, D100, add, d1, succ, toInt, toInt')
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

infixr 2 addRows as |||
infixr 3 addCols as ++

data Layout
  = NoBorders
  | HeaderLineOnly
  | LightHorizontalLines

showLayout ∷ Layout → String
showLayout = case _ of
  NoBorders → "noBorders"
  HeaderLineOnly → "headerLineOnly"
  LightHorizontalLines → "lightHorizontalLines"

mkTable_ ∷ ∀ a w. Layout → TableBody a w → PrimTable a
mkTable_ layout (TableBody body) = PrimTable
  { body, layout, widths: Nothing }

mkTable ∷
  ∀ a w tup sum
  . SumTuple tup sum
  ⇒ LtEq sum D100
  ⇒ NatTupleToIntArray tup
  ⇒ TupleLength tup w
  ⇒ Layout
  → tup
  → TableBody a w
  → PrimTable a
mkTable layout tup (TableBody body) = PrimTable
  { body, layout, widths: Just $ natTupleToIntArray tup }

class TupleLength tup len | tup → len where
  tupleLength ∷ tup → len
instance tupleLengthCons
    ∷ ( TupleLength tail tailLen
      , Succ tailLen len
      )
    ⇒ TupleLength (Tuple a tail) len
  where
  tupleLength (Tuple _ tail) = succ $ tupleLength tail
else instance tupleLengthHead ∷ TupleLength a D1 where
  tupleLength _ = d1

class SumTuple tup res | tup → res where
  sumTuple ∷ tup → res
instance sumCons
    ∷ ( Nat n
      , SumTuple tail t
      , Add n t res
      )
    ⇒ SumTuple (Tuple n tail) res
  where
  sumTuple (Tuple n tail) = add n $ sumTuple tail
else instance sumHead ∷ Nat n ⇒ SumTuple n n where
  sumTuple n = n

class NatTupleToIntArray tup where
  natTupleToIntArray ∷ tup → Array Int
instance natTupleToIntArrayCons
    ∷ ( Nat n
      , NatTupleToIntArray tail
      )
    ⇒ NatTupleToIntArray (Tuple n tail)
  where
  natTupleToIntArray (Tuple n tail) = toInt n : natTupleToIntArray tail
else instance natTupleToIntArrayHead ∷ Nat n ⇒ NatTupleToIntArray n where
  natTupleToIntArray n = [toInt n]

newtype PrimTable a = PrimTable 
  { body ∷ Array (Array (PrimCell a))
  , layout ∷ Layout
  , widths ∷ Maybe (Array Int)
  }


-- AbstractCell

newtype Cell a colSpan rowSpan = Cell { value ∷ a }

data PrimCell a
  = Empty
  | PrimCell
    { value ∷ a
    , rowSpan ∷ Int
    , colSpan ∷ Int
    }


-- TableBodyH

newtype TableBodyH a width height = TableBodyH (Array (Array (PrimCell a)))

fromCell ∷
  ∀ a w h
  . Nat w
  ⇒ Nat h
  ⇒ LtEq D1 w
  ⇒ LtEq D1 h
  ⇒ Cell a w h
  → TableBodyH a w h
fromCell (Cell { value }) = TableBodyH rows
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
  ⇒ TableBodyH a w h1
  → TableBodyH a w h2
  → TableBodyH a w h
addRows (TableBodyH t1) (TableBodyH t2) = TableBodyH $ t1 <> t2

addCols ∷ 
  ∀ a w1 w2 w h
  . Nat h
  ⇒ Add w1 w2 w
  ⇒ LtEq D1 h
  ⇒ TableBodyH a w1 h
  → TableBodyH a w2 h
  → TableBodyH a w h
addCols (TableBodyH t1) (TableBodyH t2) = TableBodyH do
  let h' = toInt' (Proxy ∷ Proxy h)
  i ← range 0 (h' - 1)
  let row1 = unsafePartial $ fromJust $ t1 !! i 
  let row2 = unsafePartial $ fromJust $ t2 !! i
  pure $ row1 <> row2


-- TableBody

toTableBody ∷
  ∀ a width height
  . TableBodyH a width height
  → TableBody a width
toTableBody (TableBodyH t) = TableBody t

newtype TableBody a width = TableBody (Array (Array (PrimCell a)))

instance tableBodySemigroup ∷ Semigroup (TableBody a w) where
  append (TableBody t1) (TableBody t2) = TableBody $ t1 <> t2

instance tableBodyMonoid ∷ Monoid (TableBody a w) where
  mempty = TableBody []


-- utils

mkCell ∷ ∀ a colSpan rowSpan. colSpan → rowSpan → a → Cell a colSpan rowSpan
mkCell _ _ value = Cell { value }

mkEmpty ∷ ∀ a w h. Nat w ⇒ Nat h ⇒ w → h → TableBodyH a w h
mkEmpty w h = TableBodyH $ replicate (toInt h) row
  where row = replicate (toInt w) Empty
