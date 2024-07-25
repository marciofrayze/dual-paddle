module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode


type alias Model =
    { count : Float
    , player : Player
    , ball : Ball
    }


type Msg
    = Frame Float
    | KeyDown Key
    | KeyUp Key


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
    , speed : Float
    }


type alias Ball =
    { x : Float
    , y : Float
    , speed : Float
    , angle : Float
    , size : Float
    }


type Key
    = ArrowLeft
    | ArrowRight
    | UnknownKey


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { count = 0
      , player =
            { x = (gameWidth / 2) - (150 / 2)
            , y = gameHeight - 30
            , moving = NotMoving
            , width = 150
            , height = 15
            , speed = 0.3
            }
      , ball =
            { x = (gameWidth / 2) - (5 / 2)
            , y = gameHeight - 40
            , speed = 0.1
            , angle = 4.71239 -- 270 degrees in radians
            , size = 5
            }
      }
    , Cmd.none
    )


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
            , renderGame model.count model.player model.ball
            ]
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) gameWidth gameHeight ]


playerShape : Player -> Shape
playerShape player =
    rect ( player.x, player.y ) player.width player.height


ballShape : Ball -> Shape
ballShape ball =
    circle ( ball.x, ball.y ) ball.size


renderGame : Float -> Player -> Ball -> Renderable
renderGame count player ball =
    let
        gameShapes =
            shapes []
                [ playerShape player
                , ballShape ball
                ]

        frameCountText =
            text [] ( 50, 50 ) (String.fromFloat count)
    in
    group [] [ gameShapes, frameCountText ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onKeyDown (Decode.map KeyDown keyDecoder)
        , onKeyUp (Decode.map KeyUp keyDecoder)
        ]


keyDecoder : Decode.Decoder Key
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.map
            (\key ->
                case key of
                    "ArrowLeft" ->
                        ArrowLeft

                    "ArrowRight" ->
                        ArrowRight

                    _ ->
                        UnknownKey
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        canMoveLeft player =
            player.x > 0

        canMoveRight player =
            player.x + player.width < gameWidth

        updatePlayerPosition player movement =
            case movement of
                MovingLeft ->
                    if canMoveLeft player then
                        { player | moving = movement }

                    else
                        { player | moving = NotMoving }

                MovingRight ->
                    if canMoveRight player then
                        { player | moving = movement }

                    else
                        { player | moving = NotMoving }

                NotMoving ->
                    { player | moving = movement }
    in
    case msg of
        Frame delta ->
            let
                newPlayer =
                    case model.player.moving of
                        MovingLeft ->
                            let
                                newPlayerXPosition =
                                    model.player.x - (delta * model.player.speed)

                                updatePlayerXPosition player x =
                                    { player | x = x }
                            in
                            updatePlayerXPosition model.player newPlayerXPosition

                        MovingRight ->
                            let
                                newPlayerXPosition =
                                    model.player.x + (delta * model.player.speed)

                                updatePlayerXPosition player x =
                                    { player | x = x }
                            in
                            updatePlayerXPosition model.player newPlayerXPosition

                        NotMoving ->
                            model.player

                newBall =
                    let
                        deltaX =
                            (model.ball.speed * cos model.ball.angle) * delta

                        deltaY =
                            (model.ball.speed * sin model.ball.angle) * delta

                        updateBallPosition ball x y =
                            { ball | x = x, y = y }
                    in
                    updateBallPosition model.ball (model.ball.x + deltaX) (model.ball.y + deltaY)
            in
            ( { model | player = newPlayer, ball = newBall }, Cmd.none )

        KeyDown key ->
            case key of
                ArrowLeft ->
                    ( { model | player = updatePlayerPosition model.player MovingLeft }, Cmd.none )

                ArrowRight ->
                    ( { model | player = updatePlayerPosition model.player MovingRight }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        KeyUp key ->
            case model.player.moving of
                MovingLeft ->
                    if key == ArrowLeft then
                        ( { model | player = updatePlayerPosition model.player NotMoving }, Cmd.none )

                    else
                        ( model, Cmd.none )

                MovingRight ->
                    if key == ArrowRight then
                        ( { model | player = updatePlayerPosition model.player NotMoving }, Cmd.none )

                    else
                        ( model, Cmd.none )

                NotMoving ->
                    ( model, Cmd.none )
