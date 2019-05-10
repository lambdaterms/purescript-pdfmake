module PdfMake.Unsafe where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Nullable (Nullable, null)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign.Object (Object)

type Path = String

type DocDefinition = 
  { content ∷ Array Content
  , styles ∷ Object Style
  , defaultStyle ∷ Nullable Style
  }

type Style = 
  { fontSize ∷ Nullable Int
  , font ∷ Nullable String
  , margin ∷ Nullable (Array Int)
  -- , bold
  }

type Content = 
  { table ∷ Nullable Table
  , text ∷ Nullable String
  , rowSpan ∷ Nullable Int
  , colSpan ∷ Nullable Int
  , style ∷ Nullable String
  , layout ∷ Nullable String
  }

newtype Table = Table
  { body ∷ Array (Array Content)
  , widths ∷ Nullable (Array String)
  }

createPdf ∷ DocDefinition → Path → Aff Unit
createPdf dd path = fromEffectFnAff $ runFn2 _createPdf dd path

foreign import _createPdf ∷ Fn2 DocDefinition Path (EffectFnAff Unit)

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
  , layout: null
  }
