module Menu (init, Menu, Model, Action) where

import Html
import Html.Attributes as Attr
import Html.Events exposing (onClick)


type alias Menu a =
  { model : Model a
  , update : Action -> Model a -> Model a
  , view : Context a -> a -> Model a -> Html.Html
  }


type alias Model a =
  { title : String
  , items : List ( a, String )
  , faded : Bool
  }


type alias Context a =
  { select : Signal.Address a
  , action : Signal.Address Action
  }


type Action
  = Fade
  | Unfade


update : Action -> Model a -> Model a
update action model =
  case action of
    Fade ->
      { model | faded = True }

    Unfade ->
      { model | faded = False }


view : Context a -> a -> Model a -> Html.Html
view context selection model =
  let
    opacity =
      if model.faded == True then
        "0.5"
      else
        "1.0"

    fadeAction =
      if model.faded == True then
        Unfade
      else
        Fade

    renderItem =
      itemView context selection
  in
    Html.div
      [ Attr.style [ ( "cursor", "pointer" ) ] ]
      [ Html.div
          [ onClick context.action fadeAction
          , Attr.style [ ( "textDecoration", "underline" ) ]
          ]
          [ Html.text model.title ]
      , Html.div
          [ Attr.style [ ( "opacity", opacity ) ] ]
          (List.map renderItem model.items)
      ]


itemView : Context a -> a -> ( a, String ) -> Html.Html
itemView context selection item =
  let
    fontWeight =
      if selection == (fst item) then
        "bold"
      else
        "normal"
  in
    Html.div
      [ onClick context.select (fst item)
      , Attr.style [ ( "fontWeight", fontWeight ) ]
      ]
      [ Html.text (snd item) ]


init : String -> List ( a, String ) -> Menu a
init title items =
  let
    model =
      { title = title
      , items = items
      , faded = False
      }
  in
    { model = model
    , update = update
    , view = view
    }
