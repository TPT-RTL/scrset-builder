module Utils exposing (..)

import Html exposing (..)
import String exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, targetValue)
import Json.Decode as Json
import Types exposing (..)


numericWithDefault : a -> Result String a -> a
numericWithDefault default value =
    case value of
        Ok value ->
            value

        Err error ->
            default


parseIntWithDefault value =
    toInt value
        |> numericWithDefault 0


parseFloatWithDefault value =
    String.toFloat value
        |> numericWithDefault 0.0


unitToString : Unit -> String
unitToString unit =
    case unit of
        Pixel ->
            "px"

        ViewWidth ->
            "vw"

        Percent ->
            "%"


toUnit : String -> Unit
toUnit value =
    case value of
        "px" ->
            Pixel

        "vw" ->
            ViewWidth

        _ ->
            Percent


conditionToString : Condition -> String
conditionToString condition =
    case condition of
        MinWidth ->
            "min-width"

        MaxWidth ->
            "max-width"


formatMediaQuery : Size -> String
formatMediaQuery size =
    "("
        ++ (conditionToString size.condition)
        ++ ": "
        ++ (toString size.measure)
        ++ (unitToString size.unit)
        ++ ")"


toCondition : String -> Condition
toCondition value =
    case value of
        "min-width" ->
            MinWidth

        _ ->
            MaxWidth


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    Html.Events.on "change" (Json.map tagger targetValue)


onBlur : (String -> b) -> Attribute b
onBlur tagger =
    Html.Events.on "blur" (Json.map tagger targetValue)


updateIfFilter : (b -> b) -> (b -> Bool) -> List b -> List b
updateIfFilter updater filter list =
    case list of
        [] ->
            []

        first :: rest ->
            if (filter first) then
                (updater first) :: (updateIfFilter updater filter rest)
            else
                first :: (updateIfFilter updater filter rest)
