module TaskExample exposing (main)

import CircularBuffer
import Color
import Element exposing (Element)
import Element.Attributes exposing (..)
import Fuzz
import Fuzz.Action.Log exposing (Log)
import Fuzz.Action.Task exposing (..)
import Html exposing (..)
import Random.Pcg
import Style
import Style.Color exposing (..)
import Task


type alias TestRepr =
    List Int


type alias Model test =
    { runs : List (Log test)
    }


initialModel : Model test
initialModel =
    { runs = []
    }


type Msg test
    = RunComplete Int ( Log test, Random.Pcg.Seed )


update : Msg TestRepr -> Model TestRepr -> ( Model TestRepr, Cmd (Msg TestRepr) )
update msg model =
    case msg of
        RunComplete remaining ( log, newSeed ) ->
            ( { model | runs = log :: model.runs }
            , runNext remaining newSeed
            )


runNext : Int -> Random.Pcg.Seed -> Cmd (Msg TestRepr)
runNext remaining seed =
    if remaining <= 0 then
        Cmd.none
    else
        go seed
            |> Task.perform (RunComplete (remaining - 1))


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
                |> Task.perform (RunComplete 100)
            )
        , update = update
        , subscriptions = \model -> Sub.none
        , view =
            Element.layout defaultStyles << view
        }


view : Model test -> Element Class Variation msg
view model =
    model.runs
        -- |> List.filter (\log -> log.failure /= Nothing)
        |> List.reverse
        |> List.map viewRun
        |> Element.column TestRuns []


type Class
    = TestRuns
    | TestRun
    | Log
    | LogInit
    | LogSteps
    | LogStep
    | LogStepName
    | LogStepModel


type Variation
    = Failed
    | Passed


viewRun : Log test -> Element Class Variation msg
viewRun log =
    Element.el TestRun
        [ vary Failed (log.failure /= Nothing)
        , vary Passed (log.failure == Nothing)
        , padding 8
        ]
        (viewLog log)


viewLog : Log test -> Element Class Variation msg
viewLog log =
    let
        viewStep ( name, testModel ) =
            Element.row LogStep
                []
                [ Element.el LogStepName [ width <| fill 1 ] (Element.text name)
                , Element.el LogStepModel [ width <| fill 1 ] (Element.text <| toString testModel)
                ]
    in
    Element.column Log
        []
        [ Element.el LogInit [] (Element.text <| toString log.init)
        , Element.column LogSteps [] (List.map viewStep log.steps)
        ]


(>>>) : (b -> c -> d) -> (d -> x) -> (b -> c -> x)
(>>>) base f b c =
    f (base b c)


defaultStyles : Style.StyleSheet Class Variation
defaultStyles =
    Style.styleSheet
        [ Style.style TestRun
            [ background Color.lightGrey
            , Style.Color.text Color.charcoal
            , Style.variation Passed
                [ background Color.lightGreen
                , Style.Color.text Color.darkGreen
                ]
            , Style.variation Failed
                [ background Color.lightRed
                , Style.Color.text Color.darkRed
                ]
            ]
        ]
