import Html exposing (text, fromElement)
import Graphics.Collage exposing (..)
import Color exposing (..)
import StartApp.Simple as StartApp

main =
  StartApp.start 
    { model = {}
    , view = view
    , update = update }

update action model = 
  model
  
view address model = 
  Html.fromElement (collage 320 240 [filled Color.red (circle 50)])

