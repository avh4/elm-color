module Color exposing
    ( Color
    , rgba, rgb
    , fromRgba
    , fromHex
    , toRgba
    )

{-|


## Types

@docs Color


## Creating colors

@docs rgba, rgb
@docs fromRgba
@docs fromHex


## Converting colors to numbers

@docs toRgba

-}

import Bitwise exposing (shiftLeftBy)


{-| Represents a color.
-}
type Color
    = RgbaSpace Float Float Float Float


{-| Creates a color from RGBA (red, green, blue, alpha) values between 0.0 and 1.0 (inclusive).

The RGB values are interpretted in the [sRGB](https://en.wikipedia.org/wiki/SRGB) color space,
which is the color space specified to be meant by default by the HTML, CSS, and SVG specs.

See also: [`fromRgba`](#fromRgba)

-}
rgba : Float -> Float -> Float -> Float -> Color
rgba r g b a =
    RgbaSpace r g b a


{-| Creates a color from RGB (red, green, blue) values between 0.0 and 1.0 (inclusive).

The RGB values are interpretted in the [sRGB](https://en.wikipedia.org/wiki/SRGB) color space,
which is the color space specified to be meant by default by the HTML, CSS, and SVG specs.

See also: [`rgba`](#rgba)

-}
rgb : Float -> Float -> Float -> Color
rgb r g b =
    RgbaSpace r g b 1.0


{-| Creates a color from RGBA (red, green, blue, alpha) values between 0.0 and 1.0 (inclusive).

The RGB values are interpretted in the [sRGB](https://en.wikipedia.org/wiki/SRGB) color space,
which is the color space specified to be meant by default by the HTML, CSS, and SVG specs.

See also: [`rgba`](#rgba)

-}
fromRgba : { red : Float, green : Float, blue : Float, alpha : Float } -> Color
fromRgba { red, green, blue, alpha } =
    RgbaSpace red green blue alpha


{-| Extract the RGBA (red, green, blue, alpha) components from a `Color`.
The component values will be between 0.0 and 1.0 (inclusive).

The values produces represent the color in the [sRGB](https://en.wikipedia.org/wiki/SRGB) color space,
which is the color space specified to be meant by default by the HTML, CSS, and SVG specs.

-}
toRgba : Color -> { red : Float, green : Float, blue : Float, alpha : Float }
toRgba (RgbaSpace r g b a) =
    { red = r, green = g, blue = b, alpha = a }


{-| This function is meant for convenience of specifying colors,
and so always returns a valid color.
If the string given is not a valid 3-, 4-, 6-, or 8-digit hex string,
then this function will return `rgba 0 0 0 1`
-}
fromHex : String -> Color
fromHex hex =
    Maybe.withDefault (RgbaSpace 0 0 0 0) <|
        case String.toList hex of
            [ r1, r2, g1, g2, b1, b2 ] ->
                Maybe.map3
                    (\r g b ->
                        RgbaSpace
                            (toFloat r / 255)
                            (toFloat g / 255)
                            (toFloat b / 255)
                            1.0
                    )
                    (hex2ToInt r1 r2)
                    (hex2ToInt g1 g2)
                    (hex2ToInt b1 b2)

            _ ->
                Nothing


hex2ToInt : Char -> Char -> Maybe Int
hex2ToInt c1 c2 =
    Maybe.map2 (\v1 v2 -> shiftLeftBy 4 v1 + v2) (hexToInt c1) (hexToInt c2)


hexToInt : Char -> Maybe Int
hexToInt char =
    case Char.toLower char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        _ ->
            Nothing
