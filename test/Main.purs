module Test.Main where

import Prelude

import Data.Nullable (Nullable, notNull, null)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Foreign.Object (fromFoldable)
import PdfMake.Unsafe (Content, DocDefinition, Style, Table(..), createPdf, defaultContent, defaultStyle)
import Test.Utils (text, setStyle, table, nn)

dd1 ∷ DocDefinition
dd1 = 
  let dc = defaultContent in
  { content: 
      [ text "content 1"
      , text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam vel justo sed quam mollis accumsan. In at orci sed lectus ullamcorper tempus et nec mauris. Aliquam quis metus venenatis erat elementum aliquet at at dolor. Pellentesque non diam vel quam rhoncus dapibus. Phasellus leo mi, luctus gravida quam sed, hendrerit ornare enim. Cras condimentum risus faucibus justo gravida ultricies. Nam pellentesque lorem sit amet diam venenatis interdum. Morbi eget augue et massa vestibulum rutrum. Integer finibus dapibus tristique."
      , setStyle "tableStyle" $ table 
          [ [ text "11", text "12", text "13" ]
          , [ text "21", text "22", text "23" ]
          , [ text "31", text "32", text "33" ]
          ]
      , setStyle "tableStyle" $ table 
          [ [ text "11 & 12" # _ { colSpan = nn 2 }, dc, text "13" ]
          , [ text "21\n31" # _ { rowSpan = nn 2 }, text "22", text "23" ]
          , [ dc, text "32", text "33" ]
          ]
      ]
  , defaultStyle: nn $ defaultStyle { font = nn "Helvetica" }
  , styles: fromFoldable
      [ "tableStyle" /\ defaultStyle { margin = nn [0, 5, 0, 15] }

      ]
  }

main ∷ Effect Unit
main = launchAff_ do
  createPdf dd1 "etc/dd1.pdf"
