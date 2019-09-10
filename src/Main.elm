module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { task : String
    , tasks : List String
    }


init : Model
init =
    { task = ""
    , tasks = []
    }



-- UPDATE


type Msg
    = UpdateTask String
    | AddTask
    | RemoveAll
    | RemoveTask String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTask text ->
            { model | task = text }

        AddTask ->
            { model | tasks = model.task :: model.tasks }

        RemoveAll ->
            { model | tasks = [] }

        RemoveTask text ->
            { model
                | tasks = List.filter (\x -> x /= text) model.tasks
            }



-- VIEW


taskItem : String -> Html Msg
taskItem task =
    li []
        [ text task
        , button [ onClick (RemoveTask task) ] [ text "x" ]
        ]


tasksList : List String -> Html Msg
tasksList tasks =
    let
        child =
            List.map taskItem tasks
    in
    ul [] child


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "New Task"
            , onInput UpdateTask
            , value model.task
            ]
            []
        , button [ onClick AddTask ] [ text "Submit" ]
        , button [ onClick RemoveAll ] [ text "Remove All" ]
        , div [] [ tasksList model.tasks ]
        ]
