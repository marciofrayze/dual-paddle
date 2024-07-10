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
    }


type Key
    = ArrowLeft
    | ArrowRight
    | UnknownKey


main : Program () Model Msg
main =
    let
        initialPlayerWidth =
            150

        initialPlayerHeigth =
            15

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
        , update = update
        , subscriptions = subscriptions
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
                newModel =
                    case model.player.moving of
                        MovingLeft ->
                            let
                                newPlayerXPosition =
                                    model.player.x - (delta * 0.5)

                                updatePlayerXPosition player x =
                                    { player | x = x }
                            in
                            { model | player = updatePlayerXPosition model.player newPlayerXPosition }

                        MovingRight ->
                            let
                                newPlayerXPosition =
                                    model.player.x + (delta * 0.5)

                                updatePlayerXPosition player x =
                                    { player | x = x }
                            in
                            { model | player = updatePlayerXPosition model.player newPlayerXPosition }

                        NotMoving ->
                            model
            in
            ( newModel, Cmd.none )

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
