module Color exposing
    ( Color
    , rgb
    )

{-|


## Types

@docs Color


## Creating colors

@docs rgb

-}


{-| Represents a color.
-}
type Color
    = Color


{-| Creates a color from RGB (red, green, blue) values between 0 and 255 (inclusive).

The RGB values are interpretted in the [sRGB](https://en.wikipedia.org/wiki/SRGB) color space,
which is the color space specified to be meant by default by the HTML, CSS, and SVG specs.

-}
rgb : Int -> Int -> Int -> Color
rgb r g b =
    Color
