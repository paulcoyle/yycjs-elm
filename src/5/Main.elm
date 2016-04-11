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
  }


type Action
  = MouseMove Int Int


update action model =
  model


view address model =
  Html.div
    [ onMouseMove address MouseMove ]
    [ Html.fromElement
        (collage
          model.width
          model.height
          [ filled model.color (circle model.radius) ]
        )
    ]


onMouseMove : Signal.Address Action -> (Int -> Int -> Action) -> Html.Attribute
onMouseMove address f =
  on "mousemove"
    (object2 (\x y -> { x = x, y = y }) ("clientX" := int) ("clientY" := int))
    (\data -> message address (f data.x data.y))
