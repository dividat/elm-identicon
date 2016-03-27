module Identicon (identicon) where

{-| Generate an identicon from a string.

# Functions
@docs identicon

-}

import Svg exposing (Svg)
import Svg.Attributes as Attributes
import String
import Char
import Html


{-| Generate a identicon from a string

    main =
      identicon "Hello identicon!"
-}
identicon : String -> Html.Html
identicon string =
  let
    pixels =
      string
        |> code
        |> List.map toCoordinates
        |> (\l -> List.append l (List.map mirror l))
        |> List.map pixel
  in
    Svg.svg
      [ Attributes.viewBox "0 0 5 5"
      , Attributes.fill "blue"
      ]
      pixels




code : String -> List Int
code string =
  string
    |> String.toList
    |> List.take 15
    |> List.map Char.toCode
    |> List.indexedMap (,)
    |> List.filter (\( i, c ) -> (c % 2) == 0)
    |> List.map fst


toCoordinates : Int -> ( Int, Int )
toCoordinates i =
  let
    x =
      floor (toFloat i / 5)

    y =
      i % 5
  in
    ( x, y )


mirror : ( number, a ) -> ( number, a )
mirror ( x, y ) =
  ( 4 - x, y )


color : String -> String
color hash =
  "hsl(0, 50%, 70%)"


pixel : ( Int, Int ) -> Svg
pixel ( x, y ) =
  (Svg.rect
    [ Attributes.y (toString y)
    , Attributes.x (toString x)
    , Attributes.width "1"
    , Attributes.height "1"
    ]
    []
  )
