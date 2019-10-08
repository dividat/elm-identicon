module Main exposing (Model, Msg, iconStyles, init, inputStyles, main, styleAttribs, update, view)

import Browser
import Html exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Identicon exposing (identicon)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    String


init : Model
init =
    "Hello!"


type alias Msg =
    String


update : Msg -> Model -> Model
update text model =
    text


view : Model -> Html Msg
view model =
    let
        field =
            Html.input
                (HA.placeholder "Enter a string..."
                    :: HE.onInput identity
                    :: inputStyles
                )
                []

        icon =
            Html.div iconStyles [ identicon "200px" model ]
    in
    Html.div [] [ field, icon ]


styleAttribs : List ( String, String ) -> List (Attribute Msg)
styleAttribs styles =
    styles
        |> List.map (\( k, v ) -> HA.style k v)


inputStyles : List (Attribute Msg)
inputStyles =
    styleAttribs
        [ ( "width", "100%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        ]


iconStyles : List (Attribute Msg)
iconStyles =
    styleAttribs
        [ ( "width", "200px" )
        , ( "height", "200px" )
        , ( "padding", "50px 0" )
        , ( "margin", "auto" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        ]
