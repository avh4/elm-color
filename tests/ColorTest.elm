module ColorTest exposing (all)

import Color exposing (Color)
import Expect
import Test exposing (..)


all : Test
all =
    describe "Color"
        [ test "defines a Color type" <|
            \() ->
                let
                    color : Color
                    color =
                        Color.rgb 0 0 0
                in
                color
                    |> Expect.equal color
        ]
