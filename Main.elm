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
        div [ class "env-settings" ]
            [ h2 []
                [ span [ class "section-label-decorator" ]
                    [ text "| " ]
                , span [] [ text "Environment Settings" ]
                ]
            , div [ class "env-input-wrapper section" ]
                [ h3 [] [ text "Screen Width" ]
                , input
                    [ class "env-input"
                    , type' "range"
                    , onInput updateWidth
                    , defaultValue (toString env.width.measure)
                    , Html.Attributes.min "320"
                    , Html.Attributes.max "1920"
                    ]
                    []
                , span [ class "env-input-value" ] [ text (toString env.width.measure) ]
                ]
            , div [ class "env-input-wrapper section" ]
                [ h3 [] [ text "Screen Density" ]
                , input
                    [ class "env-input"
                    , type' "range"
                    , onInput updateDensity
                    , defaultValue (toString env.density)
                    , Html.Attributes.min "0.5"
                    , Html.Attributes.max "2"
                    , step "0.5"
                    ]
                    []
                , span [ class "env-input-value" ] [ text (toString env.density) ]
                ]
            ]


viewImageSettings : Image -> Html Msg
viewImageSettings image =
    div []
        [ h2
            []
            [ span [ class "section-label-decorator" ]
                [ text "| " ]
            , span [] [ text "Image Settings" ]
            ]
        , div [ class "section" ]
            [ h3 [] [ text "Image Candidates" ]
            , div []
                [ Html.map UpdateWidthList (WidthList.view image.widths) ]
            ]
        , div [ class "section" ]
            [ h3 [] [ text "Display Size Settings" ]
            , div []
                [ Html.map UpdateSizeList (SizeList.view image.sizes) ]
            ]
        ]


viewSizeResult : Size -> Size -> Html Msg
viewSizeResult selectedSize size =
    let
        selected =
            if (selectedSize == size) then
                span [ class "is-selected" ] [ i [ class "fa fa-check" ] [] ]
            else
                span [] []
    in
        tr []
            [ td [] [ text (formatMediaQuery size) ]
            , td [] [ text ((toString size.width.measure) ++ (unitToString size.width.unit)) ]
            , td [] [ selected ]
            ]


viewWidthResult : Size -> Width -> Width -> Html Msg
viewWidthResult selectedSize selectedWidth width =
    let
        selected =
            if (selectedWidth == width) then
                span [ class "is-selected" ] [ i [ class "fa fa-check" ] [] ]
            else
                span [] []
    in
        tr []
            [ td [] [ text (toString width.measure) ]
            , td [] [ text (toString ((Basics.toFloat width.measure) / (Basics.toFloat selectedSize.measure))) ]
            , td [] [ selected ]
            ]


viewComputedResult : Model -> Html Msg
viewComputedResult model =
    let
        selectedSize =
            findBestSize model.env.width model.image.sizes

        selectedWidth =
            findBestWidth selectedSize model.env.density model.image.widths
    in
        div []
            [ div []
                [ h2 []
                    [ span [ class "section-label-decorator" ]
                        [ text "| " ]
                    , span [] [ text "Display Size Selection" ]
                    ]
                , table [ class "result-table " ]
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
            , div []
                [ h2 []
                    [ span [ class "section-label-decorator" ]
                        [ text "| " ]
                    , span [] [ text "Width Selection" ]
                    ]
                , table [ class "result-table " ]
                    (List.append
                        [ tr []
                            [ th [] [ text "Width" ]
                            , th [] [ text "Ratio" ]
                            , th [] [ text "Selected" ]
                            ]
                        ]
                        (List.map
                            (viewWidthResult selectedSize selectedWidth)
                            model.image.widths
                        )
                    )
                ]
            ]


viewComputedImageTag : Model -> Html Msg
viewComputedImageTag model =
    let
        imageString imageWidth =
            let
                baseUrl =
                    "https://placehold.it/"

                imageHeight =
                    round (Basics.toFloat imageWidth * 9 / 16)
            in
                baseUrl ++ (toString imageWidth) ++ "x" ++ (toString imageHeight)

        widthAsString width =
            imageString width.measure
                ++ " "
                ++ (toString width.measure)
                ++ "w"

        sizeAsString size =
            (formatMediaQuery size)
                ++ " "
                ++ ((toString size.width.measure) ++ (unitToString size.width.unit))

        imageTag =
            "<img src=\""
                ++ (imageString 1920)
                ++ "\" "
                ++ "srcset=\""
                ++ (String.join ", ") (List.map widthAsString model.image.widths)
                ++ "\" "
                ++ "sizes=\""
                ++ (String.join ", ") (List.map sizeAsString model.image.sizes)
                ++ ", 100vw"
                ++ "\" "
                ++ " />"
    in
        div []
            [ h2 []
                [ span [ class "section-label-decorator" ]
                    [ text "| " ]
                , span [] [ text "Generated Image Tag" ]
                ]
            , textarea [ class "generated-tag" ] [ text imageTag ]
            ]


view : Model -> Html Msg
view model =
    let
        imgWidth value =
            value
                |> parseIntWithDefault
                |> makeImageWidth
    in
        div [ class "container" ]
            [ h1 [ class "title" ]
                [ span [ class "section-label-decorator" ]
                    [ text "| " ]
                , span [] [ text "Responsive Image Builder" ]
                ]
            , Html.form [ class "wrapper" ]
                [ table [ class "main-table" ]
                    [ tr [ class "section-row" ]
                        [ td [ class "settings-label-wrapper section-label-wrapper" ]
                            [ span [ class "settings-label section-label" ] [ text "Settings" ] ]
                        , td []
                            [ viewEnvironmentSettings model.env
                            , viewImageSettings model.image
                            ]
                        ]
                    , tr [ class "section-row" ]
                        [ td [ class "result-label-wrapper section-label-wrapper" ]
                            [ span [ class "result-label section-label" ] [ text "Results" ] ]
                        , td []
                            [ viewComputedResult model
                            , viewComputedImageTag model
                            ]
                        ]
                    ]
                ]
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
