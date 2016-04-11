import Html exposing (text, fromElement)
import Graphics.Collage exposing (..)
import Color exposing (..)
import StartApp.Simple as StartApp


main =
  StartApp.start
    { model = model
    , view = view
    , update = update
    }


model =
  { width = 320
  , height = 240
  , color = red
  , radius = 50
  }


update action model =
  model


view address model =
  Html.fromElement
    (collage
      model.width
      model.height
      [ filled model.color (circle model.radius) ]
    )
