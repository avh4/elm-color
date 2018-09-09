module Color exposing
    ( Color
    , fromRgba, rgba, rgb
    , rgb255
    , fromHsla, hsla, hsl
    , fromHex
    , toRgba
    , toHsla
    , toHex
    )

{-| Module for working with colors. Allows creating colors via either
[sRGB](https://en.wikipedia.org/wiki/RGB_color_model) values
[HSL](https://en.wikipedia.org/wiki/HSL_and_HSV) values, or
[Hex strings](https://en.wikipedia.org/wiki/Web_colors#Hex_triplet).


# Types

@docs Color


# Creating colors

All color construction functions guarantee to only construct valid color values for you.
If you happen to pass channel values that are out of range, then they will be clamped between
0.0 and 1.0, or 0 and 255 respectively.

@docs fromRgba, rgba, rgb
@docs rgb255
@docs fromHsla, hsla, hsl
@docs fromHex


# Extracing values back out of colors

@docs toRgba
@docs toHsla
@docs toHex

-}

import Bitwise exposing (shiftLeftBy)


{-| Represents a color.
-}
type Color
    = RgbaSpace Float Float Float Float


{-| Creates a color from a record of RGBA values (red, green, blue, alpha) between 0.0 and 1.0 (inclusive).

The RGB values are interpreted in the [sRGB](https://en.wikipedia.org/wiki/SRGB) color space,
which is the standard for the Internet (HTML, CSS, and SVG), as well as digital images and printing.

This is a strict function that will force you to name all channel parameters, to avoid mixing them up.

See also: [`rgba`](#rgba)

-}
fromRgba : { red : Float, green : Float, blue : Float, alpha : Float } -> Color
fromRgba { red, green, blue, alpha } =
    RgbaSpace red green blue alpha


{-| Creates a `Color` from RGBA (red, green, blue, alpha) values between 0.0 and 1.0 (inclusive).

This is a convenience function for making a color value without needing to use a record.

See also: [`fromRgba`](#fromRgba)

-}
rgba : Float -> Float -> Float -> Float -> Color
rgba r g b a =
    RgbaSpace r g b a


{-| Creates a color from RGB (red, green, blue) values between 0.0 and 1.0 (inclusive).

This is a convenience function for making a color value with full opacity.

See also: [`rgba`](#rgba)

-}
rgb : Float -> Float -> Float -> Color
rgb r g b =
    RgbaSpace r g b 1.0


{-| Creates a color from RGB (red, green, blue) 8-bit integer values between 0 and 255.

This is a convenience function if you find passing RGB channels as integers scaled to 255 more intuitive.

Note that this is less fine-grained than passing the channels as `Float` between 0.0 and 1.0, since
there are only 2^8=256 possible values for each channel.

-}
rgb255 : Int -> Int -> Int -> Color
rgb255 r g b =
    RgbaSpace (scaleFrom255 r) (scaleFrom255 g) (scaleFrom255 b) 1.0


scaleFrom255 : Int -> Float
scaleFrom255 c =
    toFloat c / 255


{-| Creates a color from [HSLA](https://en.wikipedia.org/wiki/HSL_and_HSV) (hue, saturation, lightness, alpha)
values between 0.0 and 1.0 (inclusive).

This is a strict function that will force you to name all channel parameters, to avoid mixing them up.

See also: [`hsla`](#hsla)

-}
fromHsla : { hue : Float, saturation : Float, lightness : Float, alpha : Float } -> Color
fromHsla { hue, saturation, lightness, alpha } =
    hsla hue saturation lightness alpha


{-| Creates a color from [HSLA](https://en.wikipedia.org/wiki/HSL_and_HSV) (hue, saturation, lightness, alpha)
values between 0.0 and 1.0 (inclusive).

This is a convenience function to construct colors from HSLA without needing to construct a record first.

See also: [`fromHsla`](#fromHsla)

-}
hsla : Float -> Float -> Float -> Float -> Color
hsla hue sat light alpha =
    let
        ( h, s, l ) =
            ( hue, sat, light )

        m2 =
            if l <= 0.5 then
                l * (s + 1)

            else
                l + s - l * s

        m1 =
            l * 2 - m2

        r =
            hueToRgb (h + 1 / 3)

        g =
            hueToRgb h

        b =
            hueToRgb (h - 1 / 3)

        hueToRgb h__ =
            let
                h_ =
                    if h__ < 0 then
                        h__ + 1

                    else if h__ > 1 then
                        h__ - 1

                    else
                        h__
            in
            if h_ * 6 < 1 then
                m1 + (m2 - m1) * h_ * 6

            else if h_ * 2 < 1 then
                m2

            else if h_ * 3 < 2 then
                m1 + (m2 - m1) * (2 / 3 - h_) * 6

            else
                m1
    in
    RgbaSpace r g b alpha


{-| Creates a color from [HSLA](https://en.wikipedia.org/wiki/HSL_and_HSV) (hue, saturation, lightness, alpha)
values between 0.0 and 1.0 (inclusive).

This is a convenience function to construct colors from HSL without needing to construct a record first.

See also: [`hsla`](#hsla)

-}
hsl : Float -> Float -> Float -> Color
hsl h s l =
    hsla h s l 1.0


{-| Extract the [HSLA](https://en.wikipedia.org/wiki/HSL_and_HSV) (hue, saturation, lightness, alpha)
components out of a `Color` value.
The component values will be between 0.0 and 1.0 (inclusive).
-}
toHsla : Color -> { hue : Float, saturation : Float, lightness : Float, alpha : Float }
toHsla (RgbaSpace r g b a) =
    let
        minColor =
            min r (min g b)

        maxColor =
            max r (max g b)

        h1 =
            if maxColor == r then
                (g - b) / (maxColor - minColor)

            else if maxColor == g then
                2 + (b - r) / (maxColor - minColor)

            else
                4 + (r - g) / (maxColor - minColor)

        h2 =
            h1 * (1 / 6)

        h3 =
            if isNaN h2 then
                0

            else if h2 < 0 then
                h2 + 1

            else
                h2

        l =
            (minColor + maxColor) / 2

        s =
            if minColor == maxColor then
                0

            else if l < 0.5 then
                (maxColor - minColor) / (maxColor + minColor)

            else
                (maxColor - minColor) / (2 - maxColor - minColor)
    in
    { hue = h3
    , saturation = s
    , lightness = l
    , alpha = a
    }


{-| Extract the RGBA (red, green, blue, alpha) components out of a `Color` value.
The component values will be between 0.0 and 1.0 (inclusive).

The RGB values are interpreted in the [sRGB](https://en.wikipedia.org/wiki/SRGB) color space,
which is the standard for the Internet (HTML, CSS, and SVG), as well as digital images and printing.

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
fromHex hexString =
    Maybe.withDefault (RgbaSpace 0 0 0 0) <|
        case String.toList hexString of
            [ '#', r, g, b ] ->
                fromHex8 ( r, r ) ( g, g ) ( b, b ) ( 'f', 'f' )

            [ r, g, b ] ->
                fromHex8 ( r, r ) ( g, g ) ( b, b ) ( 'f', 'f' )

            [ '#', r, g, b, a ] ->
                fromHex8 ( r, r ) ( g, g ) ( b, b ) ( a, a )

            [ r, g, b, a ] ->
                fromHex8 ( r, r ) ( g, g ) ( b, b ) ( a, a )

            [ '#', r1, r2, g1, g2, b1, b2 ] ->
                fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( 'f', 'f' )

            [ r1, r2, g1, g2, b1, b2 ] ->
                fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( 'f', 'f' )

            [ '#', r1, r2, g1, g2, b1, b2, a1, a2 ] ->
                fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 )

            [ r1, r2, g1, g2, b1, b2, a1, a2 ] ->
                fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 )

            _ ->
                Nothing


fromHex8 : ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> ( Char, Char ) -> Maybe Color
fromHex8 ( r1, r2 ) ( g1, g2 ) ( b1, b2 ) ( a1, a2 ) =
    Maybe.map4
        (\r g b a ->
            RgbaSpace
                (toFloat r / 255)
                (toFloat g / 255)
                (toFloat b / 255)
                (toFloat a / 255)
        )
        (hex2ToInt r1 r2)
        (hex2ToInt g1 g2)
        (hex2ToInt b1 b2)
        (hex2ToInt a1 a2)


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


{-| This function will convert a color to a 6-digit hexadecimal string in the format `#rrggbb`.
-}
toHex : Color -> { hex : String, alpha : Float }
toHex c =
    let
        { red, green, blue, alpha } =
            toRgba c
    in
    { hex =
        [ red, green, blue ]
            |> List.map ((*) 255)
            |> List.map round
            |> List.map int255ToHex
            |> String.concat
            |> (++) "#"
    , alpha = alpha
    }


int255ToHex : Int -> String
int255ToHex n =
    if n < 0 then
        "00"

    else if n > 255 then
        "ff"

    else
        unsafeInt255Digits n
            |> Tuple.mapBoth unsafeIntToChar unsafeIntToChar
            |> (\( a, b ) -> String.cons a (String.cons b ""))


unsafeInt255Digits : Int -> ( Int, Int )
unsafeInt255Digits n =
    let
        digit1 =
            n // 16

        digit0 =
            if digit1 /= 0 then
                modBy (digit1 * 16) n

            else
                n
    in
    ( digit1, digit0 )


unsafeIntToChar : Int -> Char
unsafeIntToChar i =
    if i < 10 then
        String.fromInt i
            |> String.uncons
            |> Maybe.map Tuple.first
            |> Maybe.withDefault '0'

    else
        case i of
            10 ->
                'a'

            11 ->
                'b'

            12 ->
                'c'

            13 ->
                'd'

            14 ->
                'e'

            15 ->
                'f'

            _ ->
                '0'
