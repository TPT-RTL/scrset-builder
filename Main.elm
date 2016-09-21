module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onBlur, targetValue)
import String exposing (..)
import Json.Decode as Json
import WidthList exposing (..)
import SizeList exposing (..)
import Types exposing (..)
import Utils exposing (..)


initialModel : Model
initialModel =
    { env =
        { width =
            { id = 0
            , measure = 640
            , unit = Pixel
            }
        , density = 1
        }
    , image =
        { widths =
            [ { id = 0
              , measure = 200
              , unit = Pixel
              }
            ]
        , sizes =
            [ { id = 0
              , condition = MinWidth
              , measure = 200
              , unit = Pixel
              , width =
                    { id = 0
                    , measure = 200
                    , unit = Pixel
                    }
              }
            ]
        }
    }


imageWidth : Width -> Html Msg
imageWidth width =
    li [] [ text ((toString width.measure) ++ (unitToString width.unit)) ]


makeEnv : Int -> Float -> Environment
makeEnv width density =
    { width =
        { id = 0
        , measure = width
        , unit = Pixel
        }
    , density = density
    }


makeImageWidth : Int -> Width
makeImageWidth width =
    { id = 0
    , measure = width
    , unit = Pixel
    }


viewEnvironmentSettings : Environment -> Html Msg
viewEnvironmentSettings env =
    let
        updateWidth value =
            value
                |> parseIntWithDefault
                |> (flip makeEnv env.density)
                |> SetEnv

        updateDensity value =
            value
                |> parseFloatWithDefault
                |> makeEnv env.width.measure
                |> SetEnv
    in
        div []
            [ h2 [] [ text "Environment Settings" ]
            , div []
                [ input
                    [ type' "range"
                    , onInput updateWidth
                    , defaultValue (toString env.width.measure)
                    , Html.Attributes.min "320"
                    , Html.Attributes.max "1920"
                    ]
                    [ text (toString env.width.measure) ]
                , span [] [ text (toString env.width.measure) ]
                , input
                    [ type' "range"
                    , onInput updateDensity
                    , defaultValue (toString env.density)
                    , Html.Attributes.min "0.5"
                    , Html.Attributes.max "2"
                    , step "0.5"
                    ]
                    []
                , span [] [ text (toString env.density) ]
                ]
            ]


viewImageSettings : Image -> Html Msg
viewImageSettings image =
    div []
        [ h2
            []
            [ text "Image Settings" ]
        , h3 [] [ text "Image Candidates" ]
        , div []
            [ Html.map UpdateWidthList (WidthList.view image.widths) ]
        , h3 [] [ text "Display Size Settings" ]
        , div []
            [ Html.map UpdateSizeList (SizeList.view image.sizes) ]
        ]


viewSizeResult : Size -> Size -> Html Msg
viewSizeResult selectedSize size =
    let
        selected =
            if (selectedSize == size) then
                "Selected"
            else
                ""
    in
        tr []
            [ td [] [ text (formatMediaQuery size) ]
            , td [] [ text ((toString size.width.measure) ++ (unitToString size.width.unit)) ]
            , td [] [ text selected ]
            ]


viewComputedResult : Model -> Html Msg
viewComputedResult model =
    let
        selectedSize =
            findBestFit model.env.width model.image.sizes
    in
        div []
            [ div []
                [ h2 [] [ text "Display Size Selection" ]
                , table []
                    (List.append
                        [ tr []
                            [ th [] [ text "Condition" ]
                            , th [] [ text "Value" ]
                            , th [] [ text "Selected" ]
                            ]
                        ]
                        (List.map
                            (viewSizeResult selectedSize)
                            model.image.sizes
                        )
                    )
                ]
            ]


view : Model -> Html Msg
view model =
    let
        imgWidth value =
            value
                |> parseIntWithDefault
                |> makeImageWidth
    in
        Html.form [ class "pure-form" ]
            [ viewEnvironmentSettings model.env
            , viewImageSettings model.image
            , viewComputedResult model
            ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetEnv env ->
            { model | env = env }

        UpdateWidthList msg ->
            let
                prevImage =
                    model.image

                image =
                    { prevImage | widths = WidthList.update msg prevImage.widths }
            in
                { model | image = image }

        UpdateSizeList msg ->
            let
                prevImage =
                    model.image

                image =
                    { prevImage | sizes = SizeList.update msg prevImage.sizes }
            in
                { model | image = image }


main : Program Never
main =
    Html.beginnerProgram
        { view = view
        , update = update
        , model = initialModel
        }
