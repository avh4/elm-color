module Color.Lab exposing (from, to)

{-| Implements conversions to and from the CIELAB color space.

For more information about this, see [[[[[[[[[[[[[[[[[[https://en.wikipedia.org/wiki/CIELAB\_color\_space#CIELAB](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB)](https://en.wikipedia.org/wiki/CIELAB_color_space#CIELAB).

For an explanation of the math in this module, see <https://observablehq.com/@mbostock/lab-and-rgb>.

-}

-- Constants


xn =
    0.96422


zn =
    0.82521


t0 =
    4 / 29


t1 =
    6 / 29


t2 =
    3 * t1 ^ 2


t3 =
    t1 ^ 3



-- TO


to : Float -> Float -> Float -> { l : Float, a : Float, b : Float }
to sr sg sb =
    let
        ( lr, lg, lb ) =
            ( srgb2lrgb sr, srgb2lrgb sg, srgb2lrgb sb )

        y =
            xyz2lab (0.2225045 * lr + 0.7168786 * lg + 0.0606169 * lb)

        ( x, z ) =
            if lr == lg && lg == lb then
                ( y, y )

            else
                ( xyz2lab ((0.4360747 * lr + 0.3850649 * lg + 0.1430804 * lb) / xn), xyz2lab ((0.0139322 * lr + 0.0971045 * lg + 0.7141733 * lb) / zn) )
    in
    { l = 116 * y - 16, a = 500 * (x - y), b = 200 * (y - z) }


{-| Converts (a coordinate) from the sRGB color space to the linear light RGB, undoing the gamma encoding of sRGB space.
-}
srgb2lrgb : Float -> Float
srgb2lrgb v =
    if v <= 0.04045 then
        v / 12.92

    else
        ((v + 0.055) / 1.055) ^ 2.4


{-| Converts (a coordinate) from the XYZ color space to the LAB color space.
-}
xyz2lab t =
    if t > t3 then
        t ^ (1 / 3)

    else
        t / t2 + t0



-- FROM


from : Float -> Float -> Float -> { red : Float, green : Float, blue : Float }
from l a b =
    let
        fl =
            (l + 16) / 116

        fa =
            if isNaN a then
                0

            else
                a / 500

        fb =
            if isNaN b then
                0

            else
                b / 200

        x =
            xn * lab2xyz (fl + fa)

        y =
            lab2xyz fl

        z =
            zn * lab2xyz (fl - fb)
    in
    { red = lrgb2srgb (3.1338561 * x - 1.6168667 * y - 0.4906146 * z)
    , green = lrgb2srgb (-0.9787684 * x + 1.9161415 * y + 0.033454 * z)
    , blue = lrgb2srgb (0.0719453 * x - 0.2289914 * y + 1.4052427 * z)
    }


{-| Converts (a coordinate) from the LAB color space to the XYZ color space.
-}
lab2xyz t =
    if t > t1 then
        t ^ 3

    else
        t2 * (t - t0)


lrgb2srgb v =
    if v <= 0.0031308 then
        12.92 * v

    else
        1.055 * (v ^ (1 / 2.4)) - 0.055
