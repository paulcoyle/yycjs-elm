module Stage8.Menu (init, view, Model) where

import Html
import Html.Attributes as Attr


type alias Model a =
  { title : String
  , items : List ( a, String )
  }


init : String -> List ( a, String ) -> Model a
init title items =
  { title = title
  , items = items
  }


view : a -> Model a -> Html.Html
view selection model =
  let
    renderItem =
      itemView selection
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


itemView : a -> ( a, String ) -> Html.Html
itemView selection item =
  let
    fontWeight =
      if selection == (fst item) then
        "bold"
      else
        "normal"
  in
    Html.div
      [ Attr.style [ ( "fontWeight", fontWeight ) ]
      ]
      [ Html.text (snd item) ]
