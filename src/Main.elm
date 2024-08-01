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
    { score : Int
    , player : Player
    , ball : Ball
    , walls : List Wall
    , highScore : Int
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


type alias Object =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias Wall =
    Object


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


initBall : Ball
initBall =
    { x = gameWidth / 2
    , y = gameHeight - 40
    , speed = 1.5
    , angle = 2
    , size = 5
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { score = 0
      , player =
            { x = (gameWidth / 2) - (150 / 2)
            , y = gameHeight - 30
            , moving = NotMoving
            , width = 150
            , height = 15
            , speed = 2
            }
      , ball =
            initBall
      , walls =
            [ { x = 0, y = 0, width = gameWidth, height = 10 }
            , { x = 0, y = 0, width = 10, height = gameHeight }
            , { x = gameWidth - 10, y = 0, width = 10, height = gameHeight }
            , { x = 100, y = 100, width = gameWidth / 4, height = 60 }
            , { x = 500, y = 100, width = gameWidth / 4, height = 60 }
            , { x = 350, y = 200, width = gameWidth / 8, height = 50 }
            ]
      , highScore = 0
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
            []
            [ clearScreen
            , renderGame model.score model.highScore model.player model.ball model.walls
            ]
        ]


clearScreen : Renderable
clearScreen =
    shapes [ fill Color.lightGray ] [ rect ( 0, 0 ) gameWidth gameHeight ]


playerShape : Player -> Shape
playerShape player =
    rect ( player.x, player.y ) player.width player.height


ballShape : Ball -> Shape
ballShape ball =
    circle ( ball.x, ball.y ) ball.size


wallShape : Wall -> Shape
wallShape wall =
    rect ( wall.x, wall.y ) wall.width wall.height


renderGame : Int -> Int -> Player -> Ball -> List Wall -> Renderable
renderGame score highScore player ball walls =
    let
        gameShapes =
            shapes []
                ([ playerShape player
                 , ballShape ball
                 ]
                    ++ List.map wallShape walls
                )

        ballAngleText =
            text [] ( 50, 100 ) ("Ball angle: " ++ String.fromFloat ball.angle)

        scoreText =
            text [] ( 50, 50 ) ("Score: " ++ String.fromInt score)

        highScoreText =
            text [] ( 50, 75 ) ("Highscore: " ++ String.fromInt highScore)
    in
    group [] [ gameShapes, scoreText, highScoreText ]


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


checkBallCollisionWith : Ball -> Object -> Bool
checkBallCollisionWith ball object =
    let
        ballLeft =
            ball.x - (ball.size / 2)

        ballRight =
            ball.x + (ball.size / 2)

        ballTop =
            ball.y - (ball.size / 2)

        ballBottom =
            ball.y + (ball.size / 2)

        objectLeft =
            object.x

        objectRight =
            object.x + object.width

        objectTop =
            object.y

        objectBottom =
            object.y + object.height
    in
    (ballRight >= objectLeft)
        && (ballLeft <= objectRight)
        && (ballBottom >= objectTop)
        && (ballTop <= objectBottom)


wallBallBouncer : Ball -> Wall -> Float
wallBallBouncer ball wall =
    let
        randomBallBounce : Float
        randomBallBounce =
            -- A "random" number between -0.1 e 0.1
            (toFloat (modBy 2 (round (ball.x * ball.y * ball.angle * ball.speed))) / 10) - 0.05

        nextBallVertically =
            { ball | y = ball.y - (ball.speed * sin ball.angle) }

        nextBallHorizontally =
            { ball | x = ball.x + (ball.speed * cos ball.angle) }

        willCollideHorizontaly =
            (checkBallCollisionWith ball wall == False)
                && checkBallCollisionWith nextBallHorizontally wall

        willCollideVerticaly =
            (checkBallCollisionWith ball wall == False)
                && checkBallCollisionWith nextBallVertically wall
    in
    if willCollideHorizontaly then
        pi - (2 * ball.angle) + randomBallBounce

    else if willCollideVerticaly then
        (2 * pi) - (2 * ball.angle) + randomBallBounce

    else
        0


playerBallBouncer : Ball -> Object -> Float
playerBallBouncer ball player =
    let
        randomBallBounce : Float
        randomBallBounce =
            -- A "random" number between -0.1 e 0.1
            (toFloat (modBy 2 (round (ball.x * ball.y * ball.angle * ball.speed))) / 10) - 0.05

        nextBallVertically =
            { ball | y = ball.y - (ball.speed * sin ball.angle) }

        nextBallHorizontally =
            { ball | x = ball.x + (ball.speed * cos ball.angle) }

        willCollideHorizontaly =
            (checkBallCollisionWith ball player == False)
                && checkBallCollisionWith nextBallHorizontally player

        willCollideVerticaly =
            (checkBallCollisionWith ball player == False)
                && checkBallCollisionWith nextBallVertically player

        -- changes the angle of the ball based on the player position, allowing the player to control the ball
        angleDueToPlayerPosition =
            let
                playerCenter =
                    player.x + (player.width / 2)

                ballCenter =
                    ball.x + (ball.size / 2)

                distanceFromCenter =
                    playerCenter - ballCenter

                maxDistanceFromCenter =
                    player.width / 2

                angleMultiplier =
                    distanceFromCenter / maxDistanceFromCenter
            in
            if distanceFromCenter == 0 then
                0

            else
                (angleMultiplier * (pi / 2)) / 3
    in
    if willCollideHorizontaly then
        pi - (2 * ball.angle) + randomBallBounce + angleDueToPlayerPosition

    else if willCollideVerticaly then
        (2 * pi) - (2 * ball.angle) + randomBallBounce + angleDueToPlayerPosition

    else
        0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        canMoveLeft player =
            -- wall width
            player.x > 10

        canMoveRight player =
            -- wall width
            player.x + player.width < gameWidth - 10

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
        Frame _ ->
            let
                playerLost =
                    model.ball.y > gameHeight + 100

                newPlayer =
                    case model.player.moving of
                        MovingLeft ->
                            let
                                newPlayerXPosition =
                                    model.player.x - model.player.speed

                                updatePlayerXPosition player x =
                                    { player | x = x }
                            in
                            updatePlayerXPosition model.player newPlayerXPosition

                        MovingRight ->
                            let
                                newPlayerXPosition =
                                    model.player.x + model.player.speed

                                updatePlayerXPosition player x =
                                    { player | x = x }
                            in
                            updatePlayerXPosition model.player newPlayerXPosition

                        NotMoving ->
                            model.player

                moveBall ball =
                    let
                        newX =
                            ball.x + (ball.speed * cos ball.angle)

                        newY =
                            ball.y - (ball.speed * sin ball.angle)

                        updateBallPosition x y =
                            { ball | x = x, y = y }
                    in
                    updateBallPosition newX newY

                wallsAngleToBounce =
                    List.map (wallBallBouncer model.ball) model.walls
                        |> List.foldl (\angle acc -> angle + acc) 0

                playerAngleToBounce =
                    -- TODO: is there a better way to handle this?
                    playerBallBouncer model.ball { x = model.player.x, y = model.player.y, width = model.player.width, height = model.player.height }

                score =
                    if wallsAngleToBounce /= 0 || playerAngleToBounce /= 0 then
                        model.score + 1

                    else
                        model.score

                newSpeed =
                    if (wallsAngleToBounce /= 0 || playerAngleToBounce /= 0) && model.ball.speed <= 4.5 then
                        model.ball.speed + 0.1

                    else
                        model.ball.speed

                bounceBall ball =
                    { ball | angle = ball.angle + wallsAngleToBounce + playerAngleToBounce, speed = newSpeed }

                fixAngle ball =
                    if ball.angle > 2 * pi then
                        { ball | angle = ball.angle - 2 * pi }

                    else if ball.angle < 0 then
                        { ball | angle = ball.angle + 2 * pi }

                    else
                        ball

                forceMinimumAngle ball =
                    if ball.angle < 0.1 then
                        { ball | angle = 0.1 }

                    else if ball.angle > 2 * pi - 0.1 then
                        { ball | angle = 2 * pi - 0.1 }

                    else
                        ball
            in
            if playerLost then
                let
                    newHighScore =
                        if model.score > model.highScore then
                            model.score

                        else
                            model.highScore
                in
                ( { model | player = newPlayer, ball = initBall, score = 0, highScore = newHighScore }, Cmd.none )

            else
                ( { model | player = newPlayer, ball = bounceBall model.ball |> moveBall |> fixAngle |> forceMinimumAngle, score = score }, Cmd.none )

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
