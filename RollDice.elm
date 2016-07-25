module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Random


-- model


type alias Model =
    { firstFace : Int
    , secondFace : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model 1 1, Cmd.none )



-- update


type Msg
    = Roll
    | NewFace ( Int, Int )



-- generate : (a -> msg) -> Generator a -> Cmd msg
-- andThen : Generator a -> (a -> Generator b) -> Generator b
-- pair : Generator a -> Generator b -> Generator (a, b)
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            let
                cmd =
                    Random.generate NewFace (Random.pair (Random.int 1 6) (Random.int 1 6))
            in
                ( model, cmd )

        NewFace ( x, y ) ->
            ( Model x y, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- view


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (toString model.firstFace), text ", ", text (toString model.secondFace) ]
        , button [ type' "button", onClick Roll ] [ text "Roll" ]
        ]


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
