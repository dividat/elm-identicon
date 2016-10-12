module Identicon exposing (identicon, identicolor)

{-| Generate an identicon from a string.

# Functions
@docs identicon, identicolor

-}

import Svg exposing (Svg)
import Svg.Attributes as Attributes
import String
import Char
import Color exposing (Color)
import Bitwise
import Html as H


{-| Generate a identicon from a string

    main =
      identicon "200px" "Hello identicon!"
-}
identicon : String -> String -> H.Html msg
identicon size string =
    let
        hash =
            computeHash string

        pixels =
            List.repeat 15 0
                |> List.indexedMap always
                |> List.filter (\i -> hash `Bitwise.shiftRight` i % 2 == 0)
                |> List.map toCoordinates
                |> (\l -> List.append l (List.map mirror l))
                |> List.map pixel
    in
        Svg.svg
            [ Attributes.viewBox "0 0 5 5"
            , Attributes.fill (color hash |> toRgbString)
            , Attributes.height size
            , Attributes.width size
            , Attributes.shapeRendering "crispEdges"
            ]
            pixels


{-| Generate a color from a string

This generates the same color that would be used in the identicon.

-}
identicolor : String -> Color
identicolor string =
    color <| computeHash string

{-| One-at-a-Time Hash

  Taken from http://www.burtleburtle.net/bob/hash/doobs.html.

-}
computeHash : String -> Int
computeHash string =
    let
        step =
            \b h ->
                h
                    |> (+) b
                    |> (\x -> x + Bitwise.shiftLeft 10 x)
                    |> (\x -> Bitwise.xor x (Bitwise.shiftRight 6 x))
    in
        string
            |> String.toList
            |> List.map Char.toCode
            |> List.foldr step 0
            |> (\x -> x + Bitwise.shiftLeft 3 x)
            |> (\x -> Bitwise.xor x (Bitwise.shiftRight 11 x))
            |> (\x -> x + Bitwise.shiftLeft 15 x)


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


color : Int -> Color
color hash =
    Color.hsl (degrees <| toFloat <| hash % 360) 0.5 0.7


toRgbString : Color -> String
toRgbString color =
    let
        rgb = Color.toRgb color

        values =
            List.map toString [ rgb.red, rgb.green, rgb.blue ]
    in
        "rgb(" ++ String.join "," values ++ ")"


pixel : ( Int, Int ) -> Svg msg
pixel ( x, y ) =
    (Svg.rect
        [ Attributes.y (toString y)
        , Attributes.x (toString x)
        , Attributes.width "1"
        , Attributes.height "1"
        ]
        []
    )
