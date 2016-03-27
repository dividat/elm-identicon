module Main (..) where

import StartApp.Simple exposing (start)
import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events
import Identicon exposing (identicon)


main : Signal Html
main =
  start
    { model = init
    , update = update
    , view = view
    }


type alias Model =
  String


init : Model
init =
  "8b1a9953c4611296a827abf8c47804d7"


update : String -> Model -> Model
update text model =
  text


view : Signal.Address String -> Model -> Html
view address model =
  let
    field =
      Html.input
        [ Attributes.placeholder "md5"
        , Events.on "input" Events.targetValue (Signal.message address)
        , inputStyle
        ]
        []

    icon =
      Html.div [ iconStyle ] [ identicon model ]
  in
    Html.div [] [ field, icon ]


inputStyle : Attribute
inputStyle =
  Attributes.style
    [ ( "width", "100%" )
    , ( "height", "40px" )
    , ( "padding", "10px 0" )
    , ( "font-size", "2em" )
    , ( "text-align", "center" )
    ]


iconStyle : Attribute
iconStyle =
  Attributes.style
    [ ( "width", "200px" )
    , ( "height", "200px" )
    , ( "padding", "50px 0" )
    , ( "margin", "auto" )
    , ( "font-size", "2em" )
    , ( "text-align", "center" )
    ]
