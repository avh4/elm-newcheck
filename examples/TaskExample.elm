module TaskExample exposing (main)

import CircularBuffer
import Fuzz
import Fuzz.Action.Log exposing (Log)
import Fuzz.Action.Task exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style)
import Random.Pcg
import Task


type alias TestRepr =
    List Int


type alias Model test =
    { runs : List (Log test)
    , remaining : Int
    }


initialModel : Model test
initialModel =
    { runs = []
    , remaining = 100
    }


type Msg test
    = RunComplete ( Log test, Random.Pcg.Seed )


update : Msg TestRepr -> Model TestRepr -> ( Model TestRepr, Cmd (Msg TestRepr) )
update msg model =
    case msg of
        RunComplete ( log, newSeed ) ->
            model
                |> addRun log
                |> runNext newSeed


addRun : Log test -> Model test -> Model test
addRun log model =
    -- if log.failure == Nothing then
    --     { model | remaining = model.remaining + 1 }
    -- else
    { model | runs = log :: model.runs }


runNext : Random.Pcg.Seed -> Model TestRepr -> ( Model TestRepr, Cmd (Msg TestRepr) )
runNext seed model =
    if model.remaining <= 0 then
        ( { model | remaining = 0 }
        , Cmd.none
        )
    else
        ( { model | remaining = model.remaining - 1 }
        , go seed
            |> Task.perform RunComplete
          -- |> (\( task, newSeed ) -> Task.perform (\x -> RunComplete ( x, newSeed ) task))
        )


go =
    Fuzz.Action.Task.test
        (CircularBuffer.empty 3)
        []
        [ readAndModify0
            { name = "get"
            , pre = \model -> not <| List.isEmpty model
            , action = CircularBuffer.get >> Task.succeed
            , test = \t -> ( List.head t, List.tail t |> Maybe.withDefault [] )

            -- TODO: combine with the precondition so we can safely destructure
            }
        , modify1
            { name = "put"
            , pre = always True
            , action = CircularBuffer.put >>> Task.succeed
            , arg = Fuzz.int
            , test = \a t -> Ok (t ++ [ a ])
            }
        ]
        >> (\( task, newSeed ) -> task |> Task.map (flip (,) newSeed))


main =
    Html.program
        { init =
            ( initialModel
            , go (Random.Pcg.initialSeed 0)
                |> Task.perform RunComplete
            )
        , update = update
        , subscriptions = \model -> Sub.none
        , view =
            \model ->
                model.runs
                    -- |> List.filter (\log -> log.failure /= Nothing)
                    |> List.reverse
                    |> List.map viewRun
                    |> Html.div []
        }


viewRun : Log test -> Html msg
viewRun log =
    div
        [ style
            [ ( "color"
              , if log.failure == Nothing then
                    "lightgray"
                else
                    "auto"
              )
            ]
        ]
        [ Html.text <| toString log ]


(>>>) : (b -> c -> d) -> (d -> x) -> (b -> c -> x)
(>>>) base f b c =
    f (base b c)
