module Color exposing
    ( Color
    , rgba, rgb
    , fromRgba
    , toRgba
    )

{-|


## Types

@docs Color


## Creating colors

@docs rgba, rgb
@docs fromRgba


## Converting colors to numbers

@docs toRgba

-}


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
