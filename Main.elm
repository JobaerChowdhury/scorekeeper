module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as App
import String exposing (..)


-- model


type alias Model =
    { players : List Player
    , playerName : String
    , playerId : Maybe Int
    , plays : List Play
    , nextId : Int
    , nextPlayId : Int
    }


type alias Common a =
    { a
        | id : Int
        , name : String
        , points : Int
    }


type alias Player =
    Common {}


type alias Play =
    Common { playerId : Int }


initialModel : Model
initialModel =
    { players = []
    , playerName = ""
    , playerId = Nothing
    , plays = []
    , nextId = 1
    , nextPlayId = 1
    }


add : Model -> Model
add model =
    let
        player =
            { id = model.nextId, name = model.playerName, points = 0 }
    in
        { model
            | players = player :: model.players
            , nextId = model.nextId + 1
            , playerName = ""
        }


applyEdit : (Common a -> Bool) -> (Common a -> Common a) -> List (Common a) -> List (Common a)
applyEdit pred trans list =
    let
        filter a =
            if (pred (a)) then
                Just (trans (a))
            else
                Just a
    in
        List.filterMap filter list


edit : Model -> Int -> Model
edit model id =
    let
        updatedPlayers =
            applyEdit (\p -> p.id == id) (\p -> { p | name = model.playerName }) model.players

        updatedPlays =
            applyEdit (\p -> p.playerId == id) (\p -> { p | name = model.playerName }) model.plays
    in
        { model
            | players = updatedPlayers
            , plays = updatedPlays
            , playerName = ""
            , playerId = Nothing
        }


save : Model -> Model
save model =
    case model.playerId of
        Just val ->
            edit model val

        Nothing ->
            add model


score : Model -> Player -> Int -> Model
score model player s =
    let
        updatedPlayers =
            applyEdit (\p -> p.id == player.id) (\p -> { p | points = p.points + s }) model.players

        newPlay =
            { id = model.nextPlayId, name = player.name, playerId = player.id, points = s }

        updatedPlays =
            newPlay :: model.plays
    in
        { model
            | players = updatedPlayers
            , plays = updatedPlays
            , nextPlayId = model.nextPlayId + 1
        }


delete : Model -> Play -> Model
delete model play =
    let
        updatedPlayers =
            applyEdit (\p -> p.id == play.playerId) (\p -> { p | points = p.points - play.points }) model.players

        updatedPlays =
            List.filter (\p -> p.id /= play.id) model.plays
    in
        { model
            | players = updatedPlayers
            , plays = updatedPlays
        }



-- update


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input val ->
            { model | playerName = val }

        Edit player ->
            { model | playerId = Just player.id, playerName = player.name }

        Save ->
            if (String.isEmpty model.playerName) then
                model
            else
                save model

        Cancel ->
            { model | playerName = "", playerId = Nothing }

        Score player s ->
            score model player s

        DeletePlay play ->
            delete model play



-- view


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , showPlayers model
        , playerForm model
        , showPlays model
        ]


showPlayers : Model -> Html Msg
showPlayers model =
    div []
        [ playerListHeader
        , playerList model
        , pointsTotal model
        ]


playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerList : Model -> Html Msg
playerList model =
    model.players
        |> List.sortBy .name
        |> List.map (showPlayer model.playerId)
        |> ul []


pointsTotal : Model -> Html Msg
pointsTotal model =
    let
        total =
            List.map .points model.plays
                |> List.sum
    in
        footer []
            [ div [] [ text "Total:" ]
            , div [] [ text (toString total) ]
            ]


showPlays : Model -> Html Msg
showPlays model =
    div []
        [ playListHeading
        , playList model
        ]


playList : Model -> Html Msg
playList model =
    ul [] (List.map showPlay model.plays)


playListHeading : Html Msg
playListHeading =
    header []
        [ div [] [ text "Plays" ]
        , div [] [ text "Points" ]
        ]


showPlayer : Maybe Int -> Player -> Html Msg
showPlayer editId player =
    li []
        [ i
            [ class "edit"
            , onClick (Edit player)
            ]
            []
        , div
            [ class (editPlayerClass player editId) ]
            [ text player.name ]
        , button
            [ type' "button"
            , onClick (Score player 2)
            ]
            [ text "2pt" ]
        , button
            [ type' "button"
            , onClick (Score player 3)
            ]
            [ text "3pt" ]
        , div [] [ text (toString player.points) ]
        ]


showPlay : Play -> Html Msg
showPlay play =
    li []
        [ i
            [ class "remove"
            , onClick (DeletePlay play)
            ]
            []
        , div [] [ text play.name ]
        , div [] [ text (toString play.points) ]
        ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type' "text"
            , placeholder "Add/Edit Player..."
            , onInput Input
            , value model.playerName
            , class (editInputClass model.playerId)
            ]
            []
        , button [ type' "submit" ] [ text "Save" ]
        , button [ type' "button", onClick Cancel ] [ text "Cancel" ]
        ]


editPlayerClass : Player -> Maybe Int -> String
editPlayerClass player editId =
    addEditClass ((==) player.id) editId


editInputClass : Maybe Int -> String
editInputClass editId =
    addEditClass (\x -> True) editId


addEditClass : (Int -> Bool) -> Maybe Int -> String
addEditClass cond editId =
    case editId of
        Just x ->
            if (cond x) then
                "edit"
            else
                ""

        Nothing ->
            ""


main : Program Never
main =
    App.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
