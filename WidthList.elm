module WidthList exposing (update, view, viewWidth, newWidth, findBestWidth)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, targetValue, onWithOptions)
import Json.Decode as Json
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

        MoveWidth width oldPosition newPosition ->
            let
                listWithoutModel =
                    List.append (take oldPosition model) (drop (oldPosition + 1) model)

                left =
                    take newPosition listWithoutModel

                right =
                    drop newPosition listWithoutModel
            in
                List.append left (width :: right)

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


viewWidth : Int -> Width -> Html WidthMsg
viewWidth position width =
    td []
        [ input
            [ onInput (\value -> (UpdateWidthMeasure width.id) (parseIntWithDefault value))
            , value (toString width.measure)
            , defaultValue (toString width.measure)
            ]
            []
        ]


viewRemoveableWidth : Int -> Width -> Html WidthMsg
viewRemoveableWidth position width =
    let
        moveUp =
            MoveWidth width position (Basics.max 0 (position - 1))

        moveDown =
            MoveWidth width position (position + 1)
    in
        tr []
            (List.append
                [ viewWidth position width ]
                [ td []
                    [ a
                        [ href "#", class "button", onClickControl moveUp ]
                        [ span [] [ i [ class "fa fa-chevron-up" ] [] ] ]
                    , a
                        [ href "#", class "button", onClickControl moveDown ]
                        [ span [] [ i [ class "fa fa-chevron-down" ] [] ] ]
                    , a
                        [ href "#", class "button", onClickControl (DeleteWidth width.id) ]
                        [ span [] [ i [ class "fa fa-ban" ] [] ] ]
                    ]
                ]
            )


view : List Width -> Html WidthMsg
view widths =
    let
        widthViewList =
            List.map2 viewRemoveableWidth [0..((length widths) - 1)] widths

        header =
            tr []
                [ th [] [ text "Width" ]
                , th [ class "empty" ] [ text "" ]
                ]

        addButton =
            tr []
                [ td []
                    [ span
                        [ class "button"
                        , onClick AddWidth
                        ]
                        [ text "Add Width" ]
                    ]
                ]
    in
        table [ class "input-table" ] (List.append [ header ] (List.append widthViewList [ addButton ]))
