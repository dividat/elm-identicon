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
identicon hash =
  let
    chars =
      String.toList hash

    col1 =
      chars

    col2 =
      col1
        |> List.drop 5

    col3 =
      col2
        |> List.drop 5
  in
    Svg.svg
      [ Attributes.viewBox "0 0 5 5"
      , Attributes.fill (color hash)
      ]
      [ column 0 col1
      , column 1 col2
      , column 2 col3
      , column 3 col2
      , column 4 col1
      ]


color : String -> String
color hash =
  "hsl(0, 50%, 70%)"


column : Int -> List Char -> Svg
column x chars =
  Svg.g
    []
    (chars
      |> List.take 5
      |> List.indexedMap (,)
      |> List.map (pixel x)
      |> List.filterMap identity
    )


pixel : Int -> ( Int, Char ) -> Maybe Svg
pixel x ( y, char ) =
  let
    even =
      ((Char.toCode char) % 2) == 0
  in
    if even then
      Just
        (Svg.rect
          [ Attributes.y (toString y)
          , Attributes.x (toString x)
          , Attributes.width "1"
          , Attributes.height "1"
          ]
          []
        )
    else
      Nothing
