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
                maxId =
                    foldl Basics.max 0 (List.map .id model)
            in
                [ newSize (maxId + 1) ]
                    |> (List.append model)

        MoveSize size oldPosition newPosition ->
            let
                listWithoutModel =
                    List.append (take oldPosition model) (drop (oldPosition + 1) model)

                left =
                    take newPosition listWithoutModel

                right =
                    drop newPosition listWithoutModel
            in
                List.append left (size :: right)

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


viewSize : Int -> Size -> Html SizeMsg
viewSize position size =
    let
        updateMesaure value =
            value
                |> parseIntWithDefault
                |> UpdateSizeMeasure size.id

        moveUp =
            MoveSize size position (Basics.max 0 (position - 1))

        moveDown =
            MoveSize size position (position + 1)

        delete =
            DeleteSize size.id
    in
        tr []
            [ td [] [ viewCondition size ]
            , td []
                [ input
                    [ onInput updateMesaure
                    , value (toString size.measure)
                    , defaultValue (toString size.measure)
                    ]
                    []
                ]
              -- , td [] [ viewUnit size ]
            , (Html.map (UpdateSizeWidth size.id) (WidthList.viewWidth size.width.id size.width))
            , td []
                [ a
                    [ href "#", class "button", onClickControl moveUp ]
                    [ span [] [ i [ class "fa fa-chevron-up" ] [] ] ]
                , a
                    [ href "#", class "button", onClickControl moveDown ]
                    [ span [] [ i [ class "fa fa-chevron-down" ] [] ] ]
                , a
                    [ href "#", class "button", onClickControl delete ]
                    [ span [] [ i [ class "fa fa-ban" ] [] ] ]
                ]
            ]


view : List Size -> Html SizeMsg
view sizes =
    let
        sizeElements =
            List.map2 viewSize [0..((length sizes) - 1)] sizes

        header =
            tr []
                [ th [] [ text "Condition" ]
                , th [] [ text "Condition Width" ]
                  -- , th [] [ text "Unit" ]
                , th [] [ text "Render Width" ]
                , th [ class "empty" ] [ text "" ]
                ]

        addButton =
            tr []
                [ td []
                    [ span
                        [ onClick AddSize, class "button" ]
                        [ text "Add Size" ]
                    ]
                ]
    in
        table [ class "input-table" ] (List.append [ header ] (List.append sizeElements [ addButton ]))
