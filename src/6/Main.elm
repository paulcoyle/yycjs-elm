module Main (..) where

import Html exposing (text, fromElement)
import Html.Events exposing (on)
import Json.Decode exposing ((:=), int, object2)
import Signal exposing (message)
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
  , px = 0
  , py = 0
  }


type Action
  = MouseMove Int Int


update action model =
  case action of
    MouseMove x y ->
      { model
        | px = toFloat (x - model.width // 2)
        , py = toFloat (model.height // 2 - y) }


view address model =
  Html.div
    [ onMouseMove address MouseMove ]
    [ Html.fromElement
        (collage
          model.width
          model.height
          [ move (model.px, model.py) (filled model.color (circle model.radius)) ]
        )
    ]


onMouseMove : Signal.Address Action -> (Int -> Int -> Action) -> Html.Attribute
onMouseMove address f =
  on "mousemove"
    (object2 (\x y -> { x = x, y = y }) ("clientX" := int) ("clientY" := int))
    (\data -> message address (f data.x data.y))
