import Html exposing (text, fromElement)
import Graphics.Collage exposing (..)
import Color exposing (..)

main =
  fromElement (collage 320 240 [filled red (circle 50)])

