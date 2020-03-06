module Util exposing (..)

import Color exposing (Color)
import Element exposing (Element)
import Html exposing (Html)
import Ionicon
import Palette.X11


joinMaybe : Maybe (Maybe a) -> Maybe a
joinMaybe =
    Maybe.withDefault Nothing


type alias RGBA =
    -- | from the Ionicon package, sadly it doesn't expose it
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


type alias IonIcon msg =
    Int -> RGBA -> Html msg


convertColor : Color -> RGBA
convertColor color =
    let
        ( r, g, b ) =
            Color.toRGB color
    in
    RGBA (r / 255) (g / 255) (b / 255) 1


icon : IonIcon msg -> (Int -> Color -> Element msg)
icon ionIcon size color =
    convertColor color
        |> ionIcon size
        |> Element.html
        |> Element.el [ Element.centerX, Element.centerY ]


defaultIcon : IonIcon msg -> Element msg
defaultIcon ionIcon =
    icon ionIcon 24 <|
        -- Color.fromRGB ( 82, 82, 82 )
        Palette.X11.dimGray
