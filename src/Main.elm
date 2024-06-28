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
    { count : Float
    , player : Player
    }


type Msg
    = Frame Float


type PlayerMovement
    = MovingLeft
    | MovingRight
    | NotMoving


type alias Player =
    { x : Float
    , y : Float
    , moving : PlayerMovement
    , width : Float
    , height : Float
    }


main : Program () Model Msg
main =
    let
        initialPlayerWidth =
            150

        initialPlayerHeigth =
            25

        initialPlayerXPosition =
            (gameWidth / 2) - (initialPlayerWidth / 2)

        initialPlayerYPosition =
            gameHeight - (initialPlayerHeigth * 1.5)
    in
    Browser.element
        { init =
            \() ->
                ( { count = 0
                  , player = { x = initialPlayerXPosition, y = initialPlayerYPosition, moving = NotMoving, width = initialPlayerWidth, height = initialPlayerHeigth }
                  }
                , Cmd.none
                )
        , view = view
        , update =
            \msg model ->
                case msg of
                    Frame _ ->
                        ( { model | count = model.count + 1 }, Cmd.none )
        , subscriptions = \model -> onAnimationFrameDelta Frame
        }


gameWidth : number
gameWidth =
    800


gameHeight : number
gameHeight =
    600


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( gameWidth, gameHeight )
            [ style "border" "10px solid rgba(0,0,0,0.7)" ]
            [ clearScreen
            , renderGame model.count model.player
            ]
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) gameWidth gameHeight ]


playerShape : Player -> Shape
playerShape player =
    rect ( player.x, player.y ) player.width player.height


renderGame : Float -> Player -> Renderable
renderGame count player =
    let
        gameShapes =
            shapes [] [ playerShape player ]

        frameCountText =
            text [] ( 50, 50 ) (String.fromFloat count)
    in
    group [] [ gameShapes, frameCountText ]
