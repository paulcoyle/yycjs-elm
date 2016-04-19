module Stage10.Menu (..) where

import Html
import Html.Attributes as Attr
import Html.Events exposing (onClick)


type alias Model a =
  { title : String
  , items : List ( a, String )
  , expanded : Bool
  }


type alias Context a =
  { select : Signal.Address a
  , action : Signal.Address Action
  }


init : String -> List ( a, String ) -> Model a
init title items =
  { title = title
  , items = items
  , expanded = False
  }


type Action
  = Collapse
  | Expand


update : Action -> Model a -> Model a
update action model =
  case action of
    Collapse ->
      { model | expanded = False }

    Expand ->
      { model | expanded = True }


view : a -> Context a -> Model a -> Html.Html
view selection context model =
  let
    menuAction =
      if model.expanded then
        Collapse
      else
        Expand

    renderItem =
      itemView selection context

    itemNodes =
      if model.expanded then
        List.map renderItem model.items
      else
        []
  in
    Html.div
      [ Attr.style [ ( "cursor", "pointer" ) ]
      , onClick context.action menuAction
      ]
      [ Html.div
          [ Attr.style [ ( "textDecoration", "underline" ) ]
          ]
          [ Html.text model.title ]
      , Html.div
          []
          itemNodes
      ]


itemView : a -> Context a -> ( a, String ) -> Html.Html
itemView selection context item =
  let
    fontWeight =
      if selection == (fst item) then
        "bold"
      else
        "normal"
  in
    Html.div
      [ Attr.style [ ( "fontWeight", fontWeight ) ]
      , onClick context.select (fst item)
      ]
      [ Html.text (snd item) ]
