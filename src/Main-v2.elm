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
    { tasks : List Task
    , field : String
    , uid : Int
    }


type alias Task =
    { description : String
    , status : Bool
    , visible : Bool
    , id : Int
    }


newTask : String -> Int -> Task
newTask text id =
    { description = text
    , status = False
    , visible = True
    , id = id
    }


init : Model
init =
    { tasks = []
    , field = ""
    , uid = 0
    }



-- UPDATE


type Msg
    = UpdateField String
    | AddTask
    | DeleteAll
    | DeleteTask Int
    | ToggleStatus Int
    | DeleteCompleted
    | ShowAll
    | ShowActive
    | ShowCompleted


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateField text ->
            { model | field = text }

        AddTask ->
            { model
                | uid = model.uid + 1
                , field = ""
                , tasks =
                    if String.isEmpty model.field then
                        model.tasks

                    else
                        model.tasks ++ [ newTask model.field model.uid ]
            }

        DeleteAll ->
            { model | tasks = [] }

        DeleteTask id ->
            { model | tasks = List.filter (\x -> x.id /= id) model.tasks }

        ToggleStatus id ->
            let
                updateStatus task =
                    if task.id == id then
                        { task
                            | status =
                                case task.status of
                                    True ->
                                        False

                                    False ->
                                        True
                        }

                    else
                        task
            in
            { model
                | tasks =
                    List.map updateStatus model.tasks
            }

        DeleteCompleted ->
            { model
                | tasks =
                    model.tasks
                        |> List.filter (\x -> x.status /= True)
            }

        ShowAll ->
            let
                updateVisibility task =
                    case task.visible of
                        True ->
                            task

                        False ->
                            { task | visible = True }
            in
            { model | tasks = List.map updateVisibility model.tasks }

        ShowActive ->
            let
                updateVisibility task =
                    if task.status == False then
                        { task | visible = True }

                    else
                        { task | visible = False }
            in
            { model | tasks = List.map updateVisibility model.tasks }

        ShowCompleted ->
            let
                updateVisibility task =
                    if task.status == True then
                        { task | visible = True }

                    else
                        { task | visible = False }
            in
            { model | tasks = List.map updateVisibility model.tasks }



-- VIEW


taskItem : Task -> Html Msg
taskItem task =
    if task.visible == True then
        li []
            [ input
                [ type_ "checkbox"
                , checked <| task.status
                , onClick <| ToggleStatus task.id
                ]
                []
            , text task.description
            , button [ class "dlt", onClick <| DeleteTask task.id ] [ text "x" ]
            ]

    else
        li [] []


tasksList : List Task -> Html Msg
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
            , onInput UpdateField
            , value model.field
            ]
            []
        , button [ onClick AddTask ] [ text "Add Task" ]
        , div [] [ tasksList model.tasks ]
        , button [ onClick ShowAll ] [ text "All" ]
        , button [ onClick ShowActive ] [ text "Active" ]
        , button [ onClick ShowCompleted ] [ text "Completed" ]
        , br [] []
        , button [ onClick DeleteCompleted ] [ text "Delete Completed" ]
        ]
