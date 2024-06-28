module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)


type alias Model =
    { count : Float }


type Msg
    = Frame Float


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( { count = 0 }, Cmd.none )
        , view = view
        , update =
            \msg model ->
                case msg of
                    Frame _ ->
                        ( { model | count = model.count + 1 }, Cmd.none )
        , subscriptions = \model -> onAnimationFrameDelta Frame
        }


width : number
width =
    800


height : number
height =
    600


centerX : Float
centerX =
    width / 2


centerY : Float
centerY =
    height / 2


view : Model -> Html Msg
view { count } =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            [ clearScreen
            , render count
            ]
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


render : Float -> Renderable
render count =
    let
        size =
            width / 3

        x =
            -(size / 2)

        y =
            -(size / 2)

        myShapes =
            shapes
                [ transform
                    [ translate centerX centerY
                    , rotate (degrees (count * 3))
                    ]
                , fill (Color.hsl (degrees (count / 4)) 0.3 0.7)
                ]
                [ rect ( x, y ) size size
                ]

        myText =
            text [] ( 50, 50 ) (String.fromFloat count)
    in
    group [] [ myShapes, myText ]
