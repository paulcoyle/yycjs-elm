module Stage9.Menu (init, view, Model) where

import Html
import Html.Attributes as Attr
import Html.Events exposing (onClick)


type alias Model a =
  { title : String
  , items : List ( a, String )
  }


type alias Context a =
  { select : Signal.Address a
  }


init : String -> List ( a, String ) -> Model a
init title items =
  { title = title
  , items = items
  }


view : a -> Context a -> Model a -> Html.Html
view selection context model =
  let
    renderItem =
      itemView selection context
  in
    Html.div
      [ Attr.style [ ( "cursor", "pointer" ) ] ]
      [ Html.div
          [ Attr.style [ ( "textDecoration", "underline" ) ]
          ]
          [ Html.text model.title ]
      , Html.div
          []
          (List.map renderItem model.items)
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
