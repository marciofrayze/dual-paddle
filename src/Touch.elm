module Touch exposing (onTouchEnds, onTouchStarts)

import Html exposing (Attribute)
import Html.Events exposing (on, onClick)
import Json.Decode as Json


onTouchStarts : msg -> Attribute msg
onTouchStarts message =
    onTouchStartsMobile message


onTouchEnds : msg -> Attribute msg
onTouchEnds message =
    onTouchEndsMobile message



-- I need a custom touch event to be able to play this game with
-- 2 players on Mobile.
-- The onClick event doesn't work for this, because it cannot
-- handle multiple touches at the same time.


onTouchStartsMobile : msg -> Attribute msg
onTouchStartsMobile message =
    on "touchstart" (Json.succeed message)


onTouchEndsMobile : msg -> Attribute msg
onTouchEndsMobile message =
    on "touchend" (Json.succeed message)



-- But when debugging during development, I use this function
-- instead of onTouchStartsMobile, so I can test it on my
-- desktop.


debugOnDesktop : msg -> Attribute (Json.Decoder msg)
debugOnDesktop message =
    onClick (Json.succeed message)
