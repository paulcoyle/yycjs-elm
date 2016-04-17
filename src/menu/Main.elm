module Main (..) where

import Html exposing (text, fromElement)
import Html.Events exposing (on)
import Json.Decode exposing ((:=), int, object2)
import Signal exposing (message)
import Graphics.Collage exposing (..)
import Color exposing (..)
import StartApp.Simple as StartApp
import Menu exposing (..)


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
  , colorMenu = colorMenu
  , radiusMenu = radiusMenu
  }


colorMenu : Menu.Menu Color
colorMenu =
  Menu.init
    "Blob Colour"
    [ ( Color.red, "Red" )
    , ( Color.green, "Green" )
    , ( Color.blue, "Blue" )
    ]


radiusMenu : Menu.Menu Float
radiusMenu =
  Menu.init "Blob Radius" [ ( 10, "10" ), ( 30, "30" ), ( 50, "50" ), ( 70, "70" ) ]


type Action
  = MouseMove Int Int
  | ChangeColorMenu Menu.Action
  | ChangeRadiusMenu Menu.Action
  | ChangeColor Color
  | ChangeRadius Float


update action model =
  case action of
    MouseMove x y ->
      { model
        | px = toFloat (x - model.width // 2)
        , py = toFloat (model.height // 2 - y)
      }

    ChangeColorMenu action ->
      let
        colorMenuModel =
          model.colorMenu.update action model.colorMenu.model
      in
        { model | colorMenu = { colorMenu | model = colorMenuModel } }

    ChangeRadiusMenu action ->
      let
        radiusMenuModel =
          model.radiusMenu.update action model.radiusMenu.model
      in
        { model | radiusMenu = { radiusMenu | model = radiusMenuModel } }

    ChangeColor color ->
      { model | color = color }

    ChangeRadius radius ->
      { model | radius = radius }


view address model =
  Html.div
    []
    [ Html.div
        [ onMouseMove address MouseMove ]
        [ Html.fromElement
            (collage
              model.width
              model.height
              [ move ( model.px, model.py ) (filled model.color (circle model.radius)) ]
            )
        ]
    , colorMenuView address model
    , radiusMenuView address model
    ]


onMouseMove : Signal.Address Action -> (Int -> Int -> Action) -> Html.Attribute
onMouseMove address f =
  on
    "mousemove"
    (object2 (\x y -> { x = x, y = y }) ("clientX" := int) ("clientY" := int))
    (\data -> message address (f data.x data.y))


colorMenuView address model =
  let
    colorMenu =
      model.colorMenu

    context =
      { select = Signal.forwardTo address ChangeColor
      , action = Signal.forwardTo address ChangeColorMenu
      }
  in
    colorMenu.view context model.color colorMenu.model


radiusMenuView address model =
  let
    radiusMenu =
      model.radiusMenu

    context =
      { select = Signal.forwardTo address ChangeRadius
      , action = Signal.forwardTo address ChangeRadiusMenu
      }
  in
    radiusMenu.view context model.radius radiusMenu.model
