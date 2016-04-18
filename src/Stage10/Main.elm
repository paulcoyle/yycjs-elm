module Main (..) where

import Html exposing (text, fromElement)
import Html.Events exposing (on)
import Json.Decode exposing ((:=), int, object2)
import Signal exposing (message)
import Graphics.Collage exposing (..)
import Color exposing (..)
import StartApp.Simple as StartApp
import Stage10.Menu as Menu


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
  , food =
      [ { px = -140, py = -100, radius = 10 }
      , { px = 140, py = -100, radius = 10 }
      , { px = -140, py = 100, radius = 10 }
      , { px = 140, py = 100, radius = 10 }
      ]
  , score = 0
  , colorMenu = colorMenu
  }


colorMenu : Menu.Model Color
colorMenu =
  Menu.init
    "Blob Colour"
    [ ( Color.red, "Red" )
    , ( Color.green, "Green" )
    , ( Color.blue, "Blue" )
    ]


type Action
  = MouseMove Int Int
  | ChangeColor Color
  | ModifyColorMenu Menu.Action


update action model =
  case action of
    MouseMove x y ->
      let
        remainingFood =
          List.filter (\food -> not (collided model.px model.py model.radius food.px food.py food.radius)) model.food

        points =
          ((List.length model.food) - (List.length remainingFood)) * 100
      in
        { model
          | px = toFloat (x - model.width // 2)
          , py = toFloat (model.height // 2 - y)
          , food = remainingFood
          , score = model.score + points
        }

    ChangeColor color ->
      { model | color = color }

    ModifyColorMenu menuAction ->
      { model | colorMenu = Menu.update menuAction model.colorMenu }


collided x1 y1 r1 x2 y2 r2 =
  let
    distanceSquared =
      (x1 - x2) ^ 2 + (y1 - y2) ^ 2
  in
    distanceSquared <= ((r1 + r2) ^ 2)


view address model =
  Html.div
    [ onMouseMove address MouseMove ]
    [ Html.fromElement
        (collage
          model.width
          model.height
          (move ( model.px, model.py ) (filled model.color (circle model.radius))
            :: List.map viewFood model.food
          )
        )
    , Html.div [] [ Html.text ("Score: " ++ (toString model.score)) ]
    , viewColorMenu address model.color model.colorMenu
    ]


viewFood food =
  circle food.radius
    |> filled blue
    |> move ( food.px, food.py )


viewColorMenu : Signal.Address Action -> Color -> Menu.Model Color -> Html.Html
viewColorMenu address selected menu =
  let
    context =
      { select = Signal.forwardTo address ChangeColor
      , action = Signal.forwardTo address ModifyColorMenu
      }
  in
    Menu.view selected context menu


onMouseMove : Signal.Address Action -> (Int -> Int -> Action) -> Html.Attribute
onMouseMove address f =
  on
    "mousemove"
    (object2 (\x y -> { x = x, y = y }) ("clientX" := int) ("clientY" := int))
    (\data -> message address (f data.x data.y))
