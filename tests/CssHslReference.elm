module CssHslReference exposing (ColorInfo, RawData, all, decode, decodeExampleCell, decodeExampleCells, decodeExampleRow, decodeExamples, decodeHueDescription, decodeLightnessDescription, decodeSatCell, decodeSatRow, json, maybe, orCrash, rawToExamples, takeWhile)

{-| HSL RGB conversions from <https://www.w3.org/TR/css-color-3/#hsl-examples>
-}

import Array
import Hex
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required, requiredAt)


all : List ColorInfo
all =
    case Json.Decode.decodeString (Json.Decode.list decode) json of
        Err err ->
            Debug.todo (Json.Decode.errorToString err)

        Ok info ->
            List.concatMap rawToExamples info


type alias ColorInfo =
    { h : Int
    , s : Int
    , l : Int
    , r : Int
    , g : Int
    , b : Int
    }


rawToExamples : RawData -> List ColorInfo
rawToExamples rawData =
    let
        sats =
            Array.fromList rawData.saturations

        rowToExamples ( l, row ) =
            List.indexedMap (cellToExample l) row

        cellToExample l j ( r, g, b ) =
            { h = rawData.hue
            , s =
                Array.get j sats
                    |> orCrash ("Couldn't look up saturation: " ++ Debug.toString ( l, j, ( r, g, b ) ))
            , l = l
            , r = r
            , g = g
            , b = b
            }
    in
    List.map rowToExamples rawData.examples
        |> List.concat


orCrash : String -> Maybe a -> a
orCrash message m =
    case m of
        Nothing ->
            Debug.todo message

        Just a ->
            a


type alias RawData =
    { hue : Int -- 0 to 360
    , saturations : List Int
    , examples : List ( Int, List ( Int, Int, Int ) )
    }


decode : Decoder RawData
decode =
    Json.Decode.succeed RawData
        |> requiredAt [ "children", "0", "children", "0", "children", "1", "html" ] decodeHueDescription
        |> requiredAt [ "children", "0", "children", "2" ] decodeSatRow
        |> requiredAt [ "children", "0" ] decodeExamples


decodeSatRow : Decoder (List Int)
decodeSatRow =
    Json.Decode.at [ "children" ] (Json.Decode.list decodeSatCell)
        |> Json.Decode.map (List.filterMap identity)


decodeSatCell : Decoder (Maybe Int)
decodeSatCell =
    Json.Decode.at [ "html" ] Json.Decode.string
        |> Json.Decode.map (takeWhile Char.isDigit >> String.toInt)


decodeHueDescription : Decoder Int
decodeHueDescription =
    Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                takeWhile Char.isDigit string
                    |> String.toInt
                    |> maybe
                        (Json.Decode.fail "TODO")
                        Json.Decode.succeed
            )


decodeExamples : Decoder (List ( Int, List ( Int, Int, Int ) ))
decodeExamples =
    Json.Decode.at [ "children" ] (Json.Decode.list decodeExampleRow)
        |> Json.Decode.map (List.filterMap identity)


decodeExampleRow : Decoder (Maybe ( Int, List ( Int, Int, Int ) ))
decodeExampleRow =
    Json.Decode.succeed (Maybe.map2 Tuple.pair)
        |> requiredAt [ "children", "0", "html" ] decodeLightnessDescription
        |> requiredAt [ "children" ] decodeExampleCells


decodeExampleCells : Decoder (Maybe (List ( Int, Int, Int )))
decodeExampleCells =
    Json.Decode.list decodeExampleCell
        |> Json.Decode.map (List.filterMap identity >> Just)


decodeExampleCell : Decoder (Maybe ( Int, Int, Int ))
decodeExampleCell =
    Json.Decode.maybe
        (Json.Decode.at [ "style" ] Json.Decode.string)
        |> Json.Decode.map
            (Maybe.andThen <|
                \string ->
                    if String.startsWith "background:#" string then
                        Maybe.map3 (\a b c -> ( a, b, c ))
                            (Hex.fromString (String.slice 12 14 string |> String.toLower) |> Result.toMaybe)
                            (Hex.fromString (String.slice 14 16 string |> String.toLower) |> Result.toMaybe)
                            (Hex.fromString (String.slice 16 18 string |> String.toLower) |> Result.toMaybe)

                    else
                        Nothing
            )


decodeLightnessDescription : Decoder (Maybe Int)
decodeLightnessDescription =
    Json.Decode.string
        |> Json.Decode.map (String.trim >> String.toInt)


maybe : z -> (a -> z) -> Maybe a -> z
maybe x f =
    Maybe.map f >> Maybe.withDefault x


takeWhile : (Char -> Bool) -> String -> String
takeWhile pred string =
    let
        step acc str =
            case String.uncons str of
                Just ( first, rest ) ->
                    if pred first then
                        step (first :: acc) rest

                    else
                        acc

                Nothing ->
                    acc
    in
    step [] string
        |> List.reverse
        |> String.fromList


json : String
json =
    """[
  {"tag":"table","class":"hslexample","children":[
      {"tag":"tbody","children":[
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"0° Reds"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"Saturation"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","html":"100%"},
              {"tag":"th","html":"75%"},
              {"tag":"th","html":"50%"},
              {"tag":"th","html":"25%"},
              {"tag":"th","html":"0%"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 100"},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 88"},
              {"tag":"td","style":"background:#FFBFBF","html":""},
              {"tag":"td","style":"background:#F7C7C7","html":""},
              {"tag":"td","style":"background:#EFCFCF","html":""},
              {"tag":"td","style":"background:#E7D7D7","html":""},
              {"tag":"td","style":"background:#DFDFDF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 75"},
              {"tag":"td","style":"background:#FF8080","html":""},
              {"tag":"td","style":"background:#EF8F8F","html":""},
              {"tag":"td","style":"background:#DF9F9F","html":""},
              {"tag":"td","style":"background:#CFAFAF","html":""},
              {"tag":"td","style":"background:#BFBFBF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 63"},
              {"tag":"td","style":"background:#FF4040","html":""},
              {"tag":"td","style":"background:#E75858","html":""},
              {"tag":"td","style":"background:#CF7070","html":""},
              {"tag":"td","style":"background:#B78787","html":""},
              {"tag":"td","style":"background:#9F9F9F","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 50"},
              {"tag":"td","style":"background:#FF0000","html":""},
              {"tag":"td","style":"background:#DF2020","html":""},
              {"tag":"td","style":"background:#BF4040","html":""},
              {"tag":"td","style":"background:#9F6060","html":""},
              {"tag":"td","style":"background:#808080","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 38"},
              {"tag":"td","style":"background:#BF0000","html":""},
              {"tag":"td","style":"background:#A71818","html":""},
              {"tag":"td","style":"background:#8F3030","html":""},
              {"tag":"td","style":"background:#784848","html":""},
              {"tag":"td","style":"background:#606060","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 25"},
              {"tag":"td","style":"background:#800000","html":""},
              {"tag":"td","style":"background:#701010","html":""},
              {"tag":"td","style":"background:#602020","html":""},
              {"tag":"td","style":"background:#503030","html":""},
              {"tag":"td","style":"background:#404040","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 13"},
              {"tag":"td","style":"background:#400000","html":""},
              {"tag":"td","style":"background:#380808","html":""},
              {"tag":"td","style":"background:#301010","html":""},
              {"tag":"td","style":"background:#281818","html":""},
              {"tag":"td","style":"background:#202020","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 0"},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""}
            ]}
        ]}
    ]},
  {"tag":"table","class":"hslexample","children":[
      {"tag":"tbody","children":[
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"30° Red-Yellows (=Oranges)"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"Saturation"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","html":"100%"},
              {"tag":"th","html":"75%"},
              {"tag":"th","html":"50%"},
              {"tag":"th","html":"25%"},
              {"tag":"th","html":"0%"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 100"},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 88"},
              {"tag":"td","style":"background:#FFDFBF","html":""},
              {"tag":"td","style":"background:#F7DFC7","html":""},
              {"tag":"td","style":"background:#EFDFCF","html":""},
              {"tag":"td","style":"background:#E7DFD7","html":""},
              {"tag":"td","style":"background:#DFDFDF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 75"},
              {"tag":"td","style":"background:#FFBF80","html":""},
              {"tag":"td","style":"background:#EFBF8F","html":""},
              {"tag":"td","style":"background:#DFBF9F","html":""},
              {"tag":"td","style":"background:#CFBFAF","html":""},
              {"tag":"td","style":"background:#BFBFBF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 63"},
              {"tag":"td","style":"background:#FF9F40","html":""},
              {"tag":"td","style":"background:#E79F58","html":""},
              {"tag":"td","style":"background:#CF9F70","html":""},
              {"tag":"td","style":"background:#B79F87","html":""},
              {"tag":"td","style":"background:#9F9F9F","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 50"},
              {"tag":"td","style":"background:#FF8000","html":""},
              {"tag":"td","style":"background:#DF8020","html":""},
              {"tag":"td","style":"background:#BF8040","html":""},
              {"tag":"td","style":"background:#9F8060","html":""},
              {"tag":"td","style":"background:#808080","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 38"},
              {"tag":"td","style":"background:#BF6000","html":""},
              {"tag":"td","style":"background:#A76018","html":""},
              {"tag":"td","style":"background:#8F6030","html":""},
              {"tag":"td","style":"background:#786048","html":""},
              {"tag":"td","style":"background:#606060","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 25"},
              {"tag":"td","style":"background:#804000","html":""},
              {"tag":"td","style":"background:#704010","html":""},
              {"tag":"td","style":"background:#604020","html":""},
              {"tag":"td","style":"background:#504030","html":""},
              {"tag":"td","style":"background:#404040","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 13"},
              {"tag":"td","style":"background:#402000","html":""},
              {"tag":"td","style":"background:#382008","html":""},
              {"tag":"td","style":"background:#302010","html":""},
              {"tag":"td","style":"background:#282018","html":""},
              {"tag":"td","style":"background:#202020","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 0"},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""}
            ]}
        ]}
    ]},
  {"tag":"table","class":"hslexample","children":[
      {"tag":"tbody","children":[
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"60° Yellows"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"Saturation"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","html":"100%"},
              {"tag":"th","html":"75%"},
              {"tag":"th","html":"50%"},
              {"tag":"th","html":"25%"},
              {"tag":"th","html":"0%"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 100"},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 88"},
              {"tag":"td","style":"background:#FFFFBF","html":""},
              {"tag":"td","style":"background:#F7F7C7","html":""},
              {"tag":"td","style":"background:#EFEFCF","html":""},
              {"tag":"td","style":"background:#E7E7D7","html":""},
              {"tag":"td","style":"background:#DFDFDF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 75"},
              {"tag":"td","style":"background:#FFFF80","html":""},
              {"tag":"td","style":"background:#EFEF8F","html":""},
              {"tag":"td","style":"background:#DFDF9F","html":""},
              {"tag":"td","style":"background:#CFCFAF","html":""},
              {"tag":"td","style":"background:#BFBFBF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 63"},
              {"tag":"td","style":"background:#FFFF40","html":""},
              {"tag":"td","style":"background:#E7E758","html":""},
              {"tag":"td","style":"background:#CFCF70","html":""},
              {"tag":"td","style":"background:#B7B787","html":""},
              {"tag":"td","style":"background:#9F9F9F","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 50"},
              {"tag":"td","style":"background:#FFFF00","html":""},
              {"tag":"td","style":"background:#DFDF20","html":""},
              {"tag":"td","style":"background:#BFBF40","html":""},
              {"tag":"td","style":"background:#9F9F60","html":""},
              {"tag":"td","style":"background:#808080","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 38"},
              {"tag":"td","style":"background:#BFBF00","html":""},
              {"tag":"td","style":"background:#A7A718","html":""},
              {"tag":"td","style":"background:#8F8F30","html":""},
              {"tag":"td","style":"background:#787848","html":""},
              {"tag":"td","style":"background:#606060","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 25"},
              {"tag":"td","style":"background:#808000","html":""},
              {"tag":"td","style":"background:#707010","html":""},
              {"tag":"td","style":"background:#606020","html":""},
              {"tag":"td","style":"background:#505030","html":""},
              {"tag":"td","style":"background:#404040","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 13"},
              {"tag":"td","style":"background:#404000","html":""},
              {"tag":"td","style":"background:#383808","html":""},
              {"tag":"td","style":"background:#303010","html":""},
              {"tag":"td","style":"background:#282818","html":""},
              {"tag":"td","style":"background:#202020","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 0"},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""}
            ]}
        ]}
    ]},
  {"tag":"table","class":"hslexample","children":[
      {"tag":"tbody","children":[
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"90° Yellow-Greens"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"Saturation"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","html":"100%"},
              {"tag":"th","html":"75%"},
              {"tag":"th","html":"50%"},
              {"tag":"th","html":"25%"},
              {"tag":"th","html":"0%"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 100"},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 88"},
              {"tag":"td","style":"background:#DFFFBF","html":""},
              {"tag":"td","style":"background:#DFF7C7","html":""},
              {"tag":"td","style":"background:#DFEFCF","html":""},
              {"tag":"td","style":"background:#DFE7D7","html":""},
              {"tag":"td","style":"background:#DFDFDF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 75"},
              {"tag":"td","style":"background:#BFFF80","html":""},
              {"tag":"td","style":"background:#BFEF8F","html":""},
              {"tag":"td","style":"background:#BFDF9F","html":""},
              {"tag":"td","style":"background:#BFCFAF","html":""},
              {"tag":"td","style":"background:#BFBFBF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 63"},
              {"tag":"td","style":"background:#9FFF40","html":""},
              {"tag":"td","style":"background:#9FE758","html":""},
              {"tag":"td","style":"background:#9FCF70","html":""},
              {"tag":"td","style":"background:#9FB787","html":""},
              {"tag":"td","style":"background:#9F9F9F","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 50"},
              {"tag":"td","style":"background:#80FF00","html":""},
              {"tag":"td","style":"background:#80DF20","html":""},
              {"tag":"td","style":"background:#80BF40","html":""},
              {"tag":"td","style":"background:#809F60","html":""},
              {"tag":"td","style":"background:#808080","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 38"},
              {"tag":"td","style":"background:#60BF00","html":""},
              {"tag":"td","style":"background:#60A718","html":""},
              {"tag":"td","style":"background:#608F30","html":""},
              {"tag":"td","style":"background:#607848","html":""},
              {"tag":"td","style":"background:#606060","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 25"},
              {"tag":"td","style":"background:#408000","html":""},
              {"tag":"td","style":"background:#407010","html":""},
              {"tag":"td","style":"background:#406020","html":""},
              {"tag":"td","style":"background:#405030","html":""},
              {"tag":"td","style":"background:#404040","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 13"},
              {"tag":"td","style":"background:#204000","html":""},
              {"tag":"td","style":"background:#203808","html":""},
              {"tag":"td","style":"background:#203010","html":""},
              {"tag":"td","style":"background:#202818","html":""},
              {"tag":"td","style":"background:#202020","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 0"},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""}
            ]}
        ]}
    ]},
  {"tag":"table","class":"hslexample","children":[
      {"tag":"tbody","children":[
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"120° Greens"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"Saturation"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","html":"100%"},
              {"tag":"th","html":"75%"},
              {"tag":"th","html":"50%"},
              {"tag":"th","html":"25%"},
              {"tag":"th","html":"0%"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 100"},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 88"},
              {"tag":"td","style":"background:#BFFFBF","html":""},
              {"tag":"td","style":"background:#C7F7C7","html":""},
              {"tag":"td","style":"background:#CFEFCF","html":""},
              {"tag":"td","style":"background:#D7E7D7","html":""},
              {"tag":"td","style":"background:#DFDFDF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 75"},
              {"tag":"td","style":"background:#80FF80","html":""},
              {"tag":"td","style":"background:#8FEF8F","html":""},
              {"tag":"td","style":"background:#9FDF9F","html":""},
              {"tag":"td","style":"background:#AFCFAF","html":""},
              {"tag":"td","style":"background:#BFBFBF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 63"},
              {"tag":"td","style":"background:#40FF40","html":""},
              {"tag":"td","style":"background:#58E758","html":""},
              {"tag":"td","style":"background:#70CF70","html":""},
              {"tag":"td","style":"background:#87B787","html":""},
              {"tag":"td","style":"background:#9F9F9F","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 50"},
              {"tag":"td","style":"background:#00FF00","html":""},
              {"tag":"td","style":"background:#20DF20","html":""},
              {"tag":"td","style":"background:#40BF40","html":""},
              {"tag":"td","style":"background:#609F60","html":""},
              {"tag":"td","style":"background:#808080","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 38"},
              {"tag":"td","style":"background:#00BF00","html":""},
              {"tag":"td","style":"background:#18A718","html":""},
              {"tag":"td","style":"background:#308F30","html":""},
              {"tag":"td","style":"background:#487848","html":""},
              {"tag":"td","style":"background:#606060","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 25"},
              {"tag":"td","style":"background:#008000","html":""},
              {"tag":"td","style":"background:#107010","html":""},
              {"tag":"td","style":"background:#206020","html":""},
              {"tag":"td","style":"background:#305030","html":""},
              {"tag":"td","style":"background:#404040","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 13"},
              {"tag":"td","style":"background:#004000","html":""},
              {"tag":"td","style":"background:#083808","html":""},
              {"tag":"td","style":"background:#103010","html":""},
              {"tag":"td","style":"background:#182818","html":""},
              {"tag":"td","style":"background:#202020","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 0"},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""}
            ]}
        ]}
    ]},
  {"tag":"table","class":"hslexample","children":[
      {"tag":"tbody","children":[
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"150° Green-Cyans"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"Saturation"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","html":"100%"},
              {"tag":"th","html":"75%"},
              {"tag":"th","html":"50%"},
              {"tag":"th","html":"25%"},
              {"tag":"th","html":"0%"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 100"},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 88"},
              {"tag":"td","style":"background:#BFFFDF","html":""},
              {"tag":"td","style":"background:#C7F7DF","html":""},
              {"tag":"td","style":"background:#CFEFDF","html":""},
              {"tag":"td","style":"background:#D7E7DF","html":""},
              {"tag":"td","style":"background:#DFDFDF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 75"},
              {"tag":"td","style":"background:#80FFBF","html":""},
              {"tag":"td","style":"background:#8FEFBF","html":""},
              {"tag":"td","style":"background:#9FDFBF","html":""},
              {"tag":"td","style":"background:#AFCFBF","html":""},
              {"tag":"td","style":"background:#BFBFBF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 63"},
              {"tag":"td","style":"background:#40FF9F","html":""},
              {"tag":"td","style":"background:#58E79F","html":""},
              {"tag":"td","style":"background:#70CF9F","html":""},
              {"tag":"td","style":"background:#87B79F","html":""},
              {"tag":"td","style":"background:#9F9F9F","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 50"},
              {"tag":"td","style":"background:#00FF80","html":""},
              {"tag":"td","style":"background:#20DF80","html":""},
              {"tag":"td","style":"background:#40BF80","html":""},
              {"tag":"td","style":"background:#609F80","html":""},
              {"tag":"td","style":"background:#808080","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 38"},
              {"tag":"td","style":"background:#00BF60","html":""},
              {"tag":"td","style":"background:#18A760","html":""},
              {"tag":"td","style":"background:#308F60","html":""},
              {"tag":"td","style":"background:#487860","html":""},
              {"tag":"td","style":"background:#606060","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 25"},
              {"tag":"td","style":"background:#008040","html":""},
              {"tag":"td","style":"background:#107040","html":""},
              {"tag":"td","style":"background:#206040","html":""},
              {"tag":"td","style":"background:#305040","html":""},
              {"tag":"td","style":"background:#404040","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 13"},
              {"tag":"td","style":"background:#004020","html":""},
              {"tag":"td","style":"background:#083820","html":""},
              {"tag":"td","style":"background:#103020","html":""},
              {"tag":"td","style":"background:#182820","html":""},
              {"tag":"td","style":"background:#202020","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 0"},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""}
            ]}
        ]}
    ]},
  {"tag":"table","class":"hslexample","children":[
      {"tag":"tbody","children":[
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"180° Cyans"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"Saturation"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","html":"100%"},
              {"tag":"th","html":"75%"},
              {"tag":"th","html":"50%"},
              {"tag":"th","html":"25%"},
              {"tag":"th","html":"0%"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 100"},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 88"},
              {"tag":"td","style":"background:#BFFFFF","html":""},
              {"tag":"td","style":"background:#C7F7F7","html":""},
              {"tag":"td","style":"background:#CFEFEF","html":""},
              {"tag":"td","style":"background:#D7E7E7","html":""},
              {"tag":"td","style":"background:#DFDFDF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 75"},
              {"tag":"td","style":"background:#80FFFF","html":""},
              {"tag":"td","style":"background:#8FEFEF","html":""},
              {"tag":"td","style":"background:#9FDFDF","html":""},
              {"tag":"td","style":"background:#AFCFCF","html":""},
              {"tag":"td","style":"background:#BFBFBF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 63"},
              {"tag":"td","style":"background:#40FFFF","html":""},
              {"tag":"td","style":"background:#58E7E7","html":""},
              {"tag":"td","style":"background:#70CFCF","html":""},
              {"tag":"td","style":"background:#87B7B7","html":""},
              {"tag":"td","style":"background:#9F9F9F","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 50"},
              {"tag":"td","style":"background:#00FFFF","html":""},
              {"tag":"td","style":"background:#20DFDF","html":""},
              {"tag":"td","style":"background:#40BFBF","html":""},
              {"tag":"td","style":"background:#609F9F","html":""},
              {"tag":"td","style":"background:#808080","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 38"},
              {"tag":"td","style":"background:#00BFBF","html":""},
              {"tag":"td","style":"background:#18A7A7","html":""},
              {"tag":"td","style":"background:#308F8F","html":""},
              {"tag":"td","style":"background:#487878","html":""},
              {"tag":"td","style":"background:#606060","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 25"},
              {"tag":"td","style":"background:#008080","html":""},
              {"tag":"td","style":"background:#107070","html":""},
              {"tag":"td","style":"background:#206060","html":""},
              {"tag":"td","style":"background:#305050","html":""},
              {"tag":"td","style":"background:#404040","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 13"},
              {"tag":"td","style":"background:#004040","html":""},
              {"tag":"td","style":"background:#083838","html":""},
              {"tag":"td","style":"background:#103030","html":""},
              {"tag":"td","style":"background:#182828","html":""},
              {"tag":"td","style":"background:#202020","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 0"},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""}
            ]}
        ]}
    ]},
  {"tag":"table","class":"hslexample","children":[
      {"tag":"tbody","children":[
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"210° Cyan-Blues"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"Saturation"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","html":"100%"},
              {"tag":"th","html":"75%"},
              {"tag":"th","html":"50%"},
              {"tag":"th","html":"25%"},
              {"tag":"th","html":"0%"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 100"},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 88"},
              {"tag":"td","style":"background:#BFDFFF","html":""},
              {"tag":"td","style":"background:#C7DFF7","html":""},
              {"tag":"td","style":"background:#CFDFEF","html":""},
              {"tag":"td","style":"background:#D7DFE7","html":""},
              {"tag":"td","style":"background:#DFDFDF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 75"},
              {"tag":"td","style":"background:#80BFFF","html":""},
              {"tag":"td","style":"background:#8FBFEF","html":""},
              {"tag":"td","style":"background:#9FBFDF","html":""},
              {"tag":"td","style":"background:#AFBFCF","html":""},
              {"tag":"td","style":"background:#BFBFBF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 63"},
              {"tag":"td","style":"background:#409FFF","html":""},
              {"tag":"td","style":"background:#589FE7","html":""},
              {"tag":"td","style":"background:#709FCF","html":""},
              {"tag":"td","style":"background:#879FB7","html":""},
              {"tag":"td","style":"background:#9F9F9F","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 50"},
              {"tag":"td","style":"background:#0080FF","html":""},
              {"tag":"td","style":"background:#2080DF","html":""},
              {"tag":"td","style":"background:#4080BF","html":""},
              {"tag":"td","style":"background:#60809F","html":""},
              {"tag":"td","style":"background:#808080","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 38"},
              {"tag":"td","style":"background:#0060BF","html":""},
              {"tag":"td","style":"background:#1860A7","html":""},
              {"tag":"td","style":"background:#30608F","html":""},
              {"tag":"td","style":"background:#486078","html":""},
              {"tag":"td","style":"background:#606060","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 25"},
              {"tag":"td","style":"background:#004080","html":""},
              {"tag":"td","style":"background:#104070","html":""},
              {"tag":"td","style":"background:#204060","html":""},
              {"tag":"td","style":"background:#304050","html":""},
              {"tag":"td","style":"background:#404040","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 13"},
              {"tag":"td","style":"background:#002040","html":""},
              {"tag":"td","style":"background:#082038","html":""},
              {"tag":"td","style":"background:#102030","html":""},
              {"tag":"td","style":"background:#182028","html":""},
              {"tag":"td","style":"background:#202020","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 0"},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""}
            ]}
        ]}
    ]},
  {"tag":"table","class":"hslexample","children":[
      {"tag":"tbody","children":[
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"240° Blues"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"Saturation"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","html":"100%"},
              {"tag":"th","html":"75%"},
              {"tag":"th","html":"50%"},
              {"tag":"th","html":"25%"},
              {"tag":"th","html":"0%"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 100"},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 88"},
              {"tag":"td","style":"background:#BFBFFF","html":""},
              {"tag":"td","style":"background:#C7C7F7","html":""},
              {"tag":"td","style":"background:#CFCFEF","html":""},
              {"tag":"td","style":"background:#D7D7E7","html":""},
              {"tag":"td","style":"background:#DFDFDF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 75"},
              {"tag":"td","style":"background:#8080FF","html":""},
              {"tag":"td","style":"background:#8F8FEF","html":""},
              {"tag":"td","style":"background:#9F9FDF","html":""},
              {"tag":"td","style":"background:#AFAFCF","html":""},
              {"tag":"td","style":"background:#BFBFBF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 63"},
              {"tag":"td","style":"background:#4040FF","html":""},
              {"tag":"td","style":"background:#5858E7","html":""},
              {"tag":"td","style":"background:#7070CF","html":""},
              {"tag":"td","style":"background:#8787B7","html":""},
              {"tag":"td","style":"background:#9F9F9F","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 50"},
              {"tag":"td","style":"background:#0000FF","html":""},
              {"tag":"td","style":"background:#2020DF","html":""},
              {"tag":"td","style":"background:#4040BF","html":""},
              {"tag":"td","style":"background:#60609F","html":""},
              {"tag":"td","style":"background:#808080","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 38"},
              {"tag":"td","style":"background:#0000BF","html":""},
              {"tag":"td","style":"background:#1818A7","html":""},
              {"tag":"td","style":"background:#30308F","html":""},
              {"tag":"td","style":"background:#484878","html":""},
              {"tag":"td","style":"background:#606060","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 25"},
              {"tag":"td","style":"background:#000080","html":""},
              {"tag":"td","style":"background:#101070","html":""},
              {"tag":"td","style":"background:#202060","html":""},
              {"tag":"td","style":"background:#303050","html":""},
              {"tag":"td","style":"background:#404040","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 13"},
              {"tag":"td","style":"background:#000040","html":""},
              {"tag":"td","style":"background:#080838","html":""},
              {"tag":"td","style":"background:#101030","html":""},
              {"tag":"td","style":"background:#181828","html":""},
              {"tag":"td","style":"background:#202020","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 0"},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""}
            ]}
        ]}
    ]},
  {"tag":"table","class":"hslexample","children":[
      {"tag":"tbody","children":[
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"270° Blue-Magentas"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"Saturation"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","html":"100%"},
              {"tag":"th","html":"75%"},
              {"tag":"th","html":"50%"},
              {"tag":"th","html":"25%"},
              {"tag":"th","html":"0%"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 100"},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 88"},
              {"tag":"td","style":"background:#DFBFFF","html":""},
              {"tag":"td","style":"background:#DFC7F7","html":""},
              {"tag":"td","style":"background:#DFCFEF","html":""},
              {"tag":"td","style":"background:#DFD7E7","html":""},
              {"tag":"td","style":"background:#DFDFDF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 75"},
              {"tag":"td","style":"background:#BF80FF","html":""},
              {"tag":"td","style":"background:#BF8FEF","html":""},
              {"tag":"td","style":"background:#BF9FDF","html":""},
              {"tag":"td","style":"background:#BFAFCF","html":""},
              {"tag":"td","style":"background:#BFBFBF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 63"},
              {"tag":"td","style":"background:#9F40FF","html":""},
              {"tag":"td","style":"background:#9F58E7","html":""},
              {"tag":"td","style":"background:#9F70CF","html":""},
              {"tag":"td","style":"background:#9F87B7","html":""},
              {"tag":"td","style":"background:#9F9F9F","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 50"},
              {"tag":"td","style":"background:#8000FF","html":""},
              {"tag":"td","style":"background:#8020DF","html":""},
              {"tag":"td","style":"background:#8040BF","html":""},
              {"tag":"td","style":"background:#80609F","html":""},
              {"tag":"td","style":"background:#808080","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 38"},
              {"tag":"td","style":"background:#6000BF","html":""},
              {"tag":"td","style":"background:#6018A7","html":""},
              {"tag":"td","style":"background:#60308F","html":""},
              {"tag":"td","style":"background:#604878","html":""},
              {"tag":"td","style":"background:#606060","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 25"},
              {"tag":"td","style":"background:#400080","html":""},
              {"tag":"td","style":"background:#401070","html":""},
              {"tag":"td","style":"background:#402060","html":""},
              {"tag":"td","style":"background:#403050","html":""},
              {"tag":"td","style":"background:#404040","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 13"},
              {"tag":"td","style":"background:#200040","html":""},
              {"tag":"td","style":"background:#200838","html":""},
              {"tag":"td","style":"background:#201030","html":""},
              {"tag":"td","style":"background:#201828","html":""},
              {"tag":"td","style":"background:#202020","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 0"},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""}
            ]}
        ]}
    ]},
  {"tag":"table","class":"hslexample","children":[
      {"tag":"tbody","children":[
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"300° Magentas"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"Saturation"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","html":"100%"},
              {"tag":"th","html":"75%"},
              {"tag":"th","html":"50%"},
              {"tag":"th","html":"25%"},
              {"tag":"th","html":"0%"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 100"},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 88"},
              {"tag":"td","style":"background:#FFBFFF","html":""},
              {"tag":"td","style":"background:#F7C7F7","html":""},
              {"tag":"td","style":"background:#EFCFEF","html":""},
              {"tag":"td","style":"background:#E7D7E7","html":""},
              {"tag":"td","style":"background:#DFDFDF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 75"},
              {"tag":"td","style":"background:#FF80FF","html":""},
              {"tag":"td","style":"background:#EF8FEF","html":""},
              {"tag":"td","style":"background:#DF9FDF","html":""},
              {"tag":"td","style":"background:#CFAFCF","html":""},
              {"tag":"td","style":"background:#BFBFBF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 63"},
              {"tag":"td","style":"background:#FF40FF","html":""},
              {"tag":"td","style":"background:#E758E7","html":""},
              {"tag":"td","style":"background:#CF70CF","html":""},
              {"tag":"td","style":"background:#B787B7","html":""},
              {"tag":"td","style":"background:#9F9F9F","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 50"},
              {"tag":"td","style":"background:#FF00FF","html":""},
              {"tag":"td","style":"background:#DF20DF","html":""},
              {"tag":"td","style":"background:#BF40BF","html":""},
              {"tag":"td","style":"background:#9F609F","html":""},
              {"tag":"td","style":"background:#808080","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 38"},
              {"tag":"td","style":"background:#BF00BF","html":""},
              {"tag":"td","style":"background:#A718A7","html":""},
              {"tag":"td","style":"background:#8F308F","html":""},
              {"tag":"td","style":"background:#784878","html":""},
              {"tag":"td","style":"background:#606060","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 25"},
              {"tag":"td","style":"background:#800080","html":""},
              {"tag":"td","style":"background:#701070","html":""},
              {"tag":"td","style":"background:#602060","html":""},
              {"tag":"td","style":"background:#503050","html":""},
              {"tag":"td","style":"background:#404040","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 13"},
              {"tag":"td","style":"background:#400040","html":""},
              {"tag":"td","style":"background:#380838","html":""},
              {"tag":"td","style":"background:#301030","html":""},
              {"tag":"td","style":"background:#281828","html":""},
              {"tag":"td","style":"background:#202020","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 0"},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""}
            ]}
        ]}
    ]},
  {"tag":"table","class":"hslexample","children":[
      {"tag":"tbody","children":[
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"330° Magenta-Reds"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","colspan":"5","html":"Saturation"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":""},
              {"tag":"th","html":"100%"},
              {"tag":"th","html":"75%"},
              {"tag":"th","html":"50%"},
              {"tag":"th","html":"25%"},
              {"tag":"th","html":"0%"}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 100"},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""},
              {"tag":"td","style":"background:#FFFFFF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 88"},
              {"tag":"td","style":"background:#FFBFDF","html":""},
              {"tag":"td","style":"background:#F7C7DF","html":""},
              {"tag":"td","style":"background:#EFCFDF","html":""},
              {"tag":"td","style":"background:#E7D7DF","html":""},
              {"tag":"td","style":"background:#DFDFDF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 75"},
              {"tag":"td","style":"background:#FF80BF","html":""},
              {"tag":"td","style":"background:#EF8FBF","html":""},
              {"tag":"td","style":"background:#DF9FBF","html":""},
              {"tag":"td","style":"background:#CFAFBF","html":""},
              {"tag":"td","style":"background:#BFBFBF","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 63"},
              {"tag":"td","style":"background:#FF409F","html":""},
              {"tag":"td","style":"background:#E7589F","html":""},
              {"tag":"td","style":"background:#CF709F","html":""},
              {"tag":"td","style":"background:#B7879F","html":""},
              {"tag":"td","style":"background:#9F9F9F","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 50"},
              {"tag":"td","style":"background:#FF0080","html":""},
              {"tag":"td","style":"background:#DF2080","html":""},
              {"tag":"td","style":"background:#BF4080","html":""},
              {"tag":"td","style":"background:#9F6080","html":""},
              {"tag":"td","style":"background:#808080","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 38"},
              {"tag":"td","style":"background:#BF0060","html":""},
              {"tag":"td","style":"background:#A71860","html":""},
              {"tag":"td","style":"background:#8F3060","html":""},
              {"tag":"td","style":"background:#784860","html":""},
              {"tag":"td","style":"background:#606060","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 25"},
              {"tag":"td","style":"background:#800040","html":""},
              {"tag":"td","style":"background:#701040","html":""},
              {"tag":"td","style":"background:#602040","html":""},
              {"tag":"td","style":"background:#503040","html":""},
              {"tag":"td","style":"background:#404040","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 13"},
              {"tag":"td","style":"background:#400020","html":""},
              {"tag":"td","style":"background:#380820","html":""},
              {"tag":"td","style":"background:#301020","html":""},
              {"tag":"td","style":"background:#281820","html":""},
              {"tag":"td","style":"background:#202020","html":""}
            ]},
          {"tag":"tr","children":[
              {"tag":"th","html":" 0"},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""},
              {"tag":"td","style":"background:#000000","html":""}
            ]}
        ]}
    ]}
]
"""
