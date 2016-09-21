module SizeList exposing (update, view, findBestSize)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, targetValue)
import List exposing (..)
import Maybe exposing (..)
import Types exposing (..)
import Utils exposing (..)
import WidthList exposing (..)


--type SizeMsg
--    = AddSize
--    | DeleteSize Int
--    | UpdateSizeCondition Condition
--    | UpdateSizeMeasure Int
--    | UpdateSizeUnit Unit


newSize : Int -> Size
newSize id =
    { id = id
    , condition = MinWidth
    , measure = 0
    , unit = Pixel
    , width = WidthList.newWidth 0
    }


findBestSize : Width -> List Size -> Size
findBestSize width sizes =
    let
        checkMinWidth size width =
            size.condition == MinWidth && size.measure <= width.measure

        checkMaxWidth size width =
            size.condition == MaxWidth && size.measure > width.measure

        filteredSizes =
            sizes
                |> List.filter (\size -> (checkMinWidth size width) || (checkMaxWidth size width))
    in
        withDefault (newSize -1) (head filteredSizes)


updateCondition : Size -> Condition -> Size
updateCondition size condition =
    { size | condition = condition }


updateMeasure : Size -> Int -> Size
updateMeasure size measure =
    { size | measure = measure }


updateUnit : Size -> Unit -> Size
updateUnit size unit =
    { size | unit = unit }


update : SizeMsg -> List Size -> List Size
update msg model =
    case msg of
        AddSize ->
            let
                lastSize =
                    model
                        |> List.reverse
                        |> List.head

                newModel =
                    case lastSize of
                        Just size ->
                            [ newSize (size.id + 1) ]

                        Nothing ->
                            [ newSize ((length model) + 1) ]
            in
                newModel
                    |> (List.append model)

        DeleteSize id ->
            List.filter (\size -> size.id /= id) model

        UpdateSizeCondition id condition ->
            let
                filter size =
                    size.id == id

                updater =
                    (flip updateCondition) condition
            in
                updateIfFilter updater filter model

        UpdateSizeMeasure id measure ->
            let
                filter size =
                    size.id == id

                updater =
                    (flip updateMeasure) measure
            in
                updateIfFilter updater filter model

        UpdateSizeUnit id unit ->
            let
                filter size =
                    size.id == id

                updater =
                    (flip updateUnit) unit
            in
                updateIfFilter updater filter model

        UpdateSizeWidth id widthMsg ->
            let
                filter size =
                    size.id == id

                updater size =
                    { size | width = withDefault (WidthList.newWidth 0) <| head <| WidthList.update widthMsg [ size.width ] }
            in
                updateIfFilter updater filter model


viewCondition : Size -> Html SizeMsg
viewCondition size =
    let
        updateCondition : String -> SizeMsg
        updateCondition value =
            value
                |> toCondition
                |> UpdateSizeCondition size.id
    in
        select
            [ onChange updateCondition ]
            [ option
                [ selected (size.condition == MinWidth)
                , value (conditionToString MinWidth)
                ]
                [ text (conditionToString MinWidth) ]
            , option
                [ selected (size.condition == MaxWidth)
                , value (conditionToString MaxWidth)
                ]
                [ text (conditionToString MaxWidth) ]
            ]


viewUnit : Size -> Html SizeMsg
viewUnit size =
    let
        updateUnit : String -> SizeMsg
        updateUnit value =
            value
                |> toUnit
                |> UpdateSizeUnit size.id

        unitOption : Unit -> Html SizeMsg
        unitOption unit =
            option
                [ selected (size.unit == unit)
                , value (unitToString unit)
                ]
                [ text (unitToString unit) ]
    in
        select
            [ onChange updateUnit ]
            (List.map unitOption [ Pixel, ViewWidth, Percent ])


viewSize : Size -> Html SizeMsg
viewSize size =
    let
        updateMesaure value =
            value
                |> parseIntWithDefault
                |> UpdateSizeMeasure size.id

        delete =
            DeleteSize size.id
    in
        li []
            [ viewCondition size
            , input
                [ onInput updateMesaure
                , defaultValue (toString size.measure)
                ]
                []
            , viewUnit size
            , (Html.map (UpdateSizeWidth size.id) (WidthList.viewWidth size.width))
            , a
                [ href "#" ]
                [ span [ class "pure-button" ] [ i [ class "fa fa-chevron-up" ] [] ] ]
            , a
                [ href "#" ]
                [ span [ class "pure-button" ] [ i [ class "fa fa-chevron-down" ] [] ] ]
            , a
                [ href "#"
                , onClick delete
                ]
                [ span [ class "pure-button" ] [ i [ class "fa fa-ban" ] [] ] ]
            ]


view : List Size -> Html SizeMsg
view sizes =
    let
        sizeElements =
            List.map viewSize sizes

        addButton =
            span
                [ class "pure-button"
                , onClick AddSize
                ]
                [ text "Add Size" ]
    in
        ul [ class "pure-menu-list" ] (List.append sizeElements [ addButton ])
