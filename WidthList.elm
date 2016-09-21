module WidthList exposing (update, view, viewWidth, newWidth)

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


updateMeasure : Width -> Int -> Width
updateMeasure width measure =
    { width | measure = measure }


update : WidthMsg -> List Width -> List Width
update msg model =
    case msg of
        AddWidth ->
            List.append model [ newWidth ((length model) + 1) ]

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
        , span [] [ text (unitToString width.unit) ]
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
                [ span [ class "pure-button" ] [ i [ class "fa fa-ban" ] [] ] ]
            ]
        )


view : List Width -> Html WidthMsg
view widths =
    let
        widthViewList =
            List.map viewRemoveableWidth widths

        addButton =
            span
                [ class "pure-button"
                , onClick AddWidth
                ]
                [ text "Add Width" ]
    in
        ul [] (List.append widthViewList [ addButton ])
