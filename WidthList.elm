module WidthList exposing (update, view, viewWidth, newWidth, findBestWidth)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, targetValue)
import List exposing (..)
import Types exposing (..)
import Utils exposing (..)


--type WidthMsg
--    = AddWidth
--    | DeleteWidth Int
--    | UpdateWidthMeasure Int Int


newWidth : Int -> Width
newWidth id =
    { id = id
    , measure = 0
    , unit = Pixel
    }


findBestWidth : Size -> Float -> List Width -> Width
findBestWidth size density widths =
    let
        toRatios size width =
            { width = width, ratio = (toFloat width.measure) / (toFloat size.width.measure) }

        withRatios =
            List.map (toRatios size)

        toDistanceFromDensity density widthWithRatio =
            { width = widthWithRatio.width, ratio = widthWithRatio.ratio, distance = widthWithRatio.ratio - density }

        withDensityDistance =
            List.map (toDistanceFromDensity density)

        chooseBestDistance dist1 dist2 =
            if (dist1.distance >= 0 && dist2.distance < 0) then
                dist1
            else if (dist1.distance < 0 && dist2.distance >= 0) then
                dist2
            else if (dist1.distance < 0 && dist2.distance < 0) then
                if (dist1.distance > dist2.distance) then
                    dist1
                else
                    dist2
            else if (dist1.distance < dist2.distance) then
                dist1
            else
                dist2

        widthOrDefault widthWithRatio =
            case widthWithRatio of
                Nothing ->
                    { width = (newWidth 0), ratio = 1, distance = 0 }

                Just widthLikeThing ->
                    widthLikeThing

        widthsWithDensities =
            widths
                -- |> (Debug.log "Raw Widths")
                |>
                    withRatios
                |> withDensityDistance

        -- |> (Debug.log "Widths with distances")
    in
        widthsWithDensities
            |> List.foldl chooseBestDistance (widthOrDefault (List.head widthsWithDensities))
            -- |> (Debug.log "Chosen width")
            |>
                .width


updateMeasure : Width -> Int -> Width
updateMeasure width measure =
    { width | measure = measure }


update : WidthMsg -> List Width -> List Width
update msg model =
    case msg of
        AddWidth ->
            let
                lastWidth =
                    model
                        |> List.reverse
                        |> List.head

                newModel =
                    case lastWidth of
                        Just width ->
                            [ newWidth (width.id + 1) ]

                        Nothing ->
                            [ newWidth ((length model) + 1) ]
            in
                newModel
                    |> (List.append model)

        DeleteWidth id ->
            List.filter (\width -> width.id /= id) model

        UpdateWidthMeasure id measure ->
            let
                filter width =
                    width.id == id

                updater =
                    (flip updateMeasure) measure
            in
                updateIfFilter updater filter model


viewWidth : Width -> Html WidthMsg
viewWidth width =
    span []
        [ input
            [ onInput (\value -> (UpdateWidthMeasure width.id) (parseIntWithDefault value))
            , defaultValue (toString width.measure)
            ]
            []
        ]


viewRemoveableWidth : Width -> Html WidthMsg
viewRemoveableWidth width =
    li []
        (List.append
            [ viewWidth width ]
            [ a
                [ href "#"
                , onClick (DeleteWidth width.id)
                ]
                [ span [ class "button" ] [ i [ class "fa fa-ban" ] [] ] ]
            ]
        )


view : List Width -> Html WidthMsg
view widths =
    let
        widthViewList =
            List.map viewRemoveableWidth widths

        addButton =
            span
                [ class "button"
                , onClick AddWidth
                ]
                [ text "Add Width" ]
    in
        ul [] (List.append widthViewList [ addButton ])
