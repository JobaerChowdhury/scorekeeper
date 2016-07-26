module Counter exposing (Model, Msg, init, view, update)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App


-- model


type alias Model =
    { count : Int
    , max : Int
    , min : Int
    }


init : Int -> Model
init i =
    Model i i i


initModel : Model
initModel =
    init 1


inc : Model -> Model
inc model =
    let
        newCount =
            model.count + 1

        maximum =
            if (newCount > model.max) then
                newCount
            else
                model.max
    in
        { model | count = newCount, max = maximum }


dec : Model -> Model
dec model =
    let
        newCount =
            model.count - 1

        minimum =
            if (newCount < model.min) then
                newCount
            else
                model.min
    in
        { model | count = newCount, min = minimum }



-- update


type Msg
    = Inc
    | Dec


update : Msg -> Model -> Model
update msg model =
    case msg of
        Inc ->
            inc model

        Dec ->
            dec model



--view


view : Model -> Html Msg
view model =
    div []
        [ button [ type' "button", onClick Dec ] [ text "-" ]
        , div [ countStyle ]
            [ text (toString model.count)
            , text ("(" ++ (toString model.max) ++ ", " ++ (toString model.min) ++ ")")
            ]
        , button [ type' "button", onClick Inc ] [ text "+" ]
        ]


countStyle : Attribute msg
countStyle =
    style
        [ ( "font-size", "20px" )
        , ( "font-family", "monospace" )
        , ( "display", "inline-block" )
        , ( "width", "150px" )
        , ( "text-align", "center" )
        ]


main : Program Never
main =
    App.beginnerProgram { model = initModel, view = view, update = update }
