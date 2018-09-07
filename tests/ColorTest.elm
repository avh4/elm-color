module ColorTest exposing (all)

import Color exposing (Color)
import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, floatRange)
import Test exposing (..)



--
-- Fuzzers
--


unit : Fuzzer Float
unit =
    floatRange 0 1


tuple2 : Fuzzer a -> Fuzzer b -> Fuzzer ( a, b )
tuple2 a b =
    Fuzz.tuple ( a, b )


tuple3 : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer ( a, b, c )
tuple3 a b c =
    Fuzz.tuple3 ( a, b, c )



--
-- Tests
--


all : Test
all =
    describe "Color"
        [ test "defines a Color type" <|
            \() ->
                let
                    color : Color
                    color =
                        Color.rgba 0 0 0 0
                in
                color
                    |> Expect.equal color
        , fuzz (tuple2 (tuple3 unit unit unit) unit)
            "can represent RGBA colors (fromRgba)"
          <|
            \( ( r, g, b ), a ) ->
                Color.fromRgba { red = r, green = g, blue = b, alpha = a }
                    |> Color.toRgba
                    |> Expect.all
                        [ .red >> Expect.within (Absolute 0.000001) r
                        , .green >> Expect.within (Absolute 0.000001) g
                        , .blue >> Expect.within (Absolute 0.000001) b
                        , .alpha >> Expect.within (Absolute 0.000001) a
                        ]
        , fuzz (tuple2 (tuple3 unit unit unit) unit)
            "can represent RGBA colors (rgba)"
          <|
            \( ( r, g, b ), a ) ->
                Color.rgba r g b a
                    |> Color.toRgba
                    |> Expect.all
                        [ .red >> Expect.within (Absolute 0.000001) r
                        , .green >> Expect.within (Absolute 0.000001) g
                        , .blue >> Expect.within (Absolute 0.000001) b
                        , .alpha >> Expect.within (Absolute 0.000001) a
                        ]
        ]
