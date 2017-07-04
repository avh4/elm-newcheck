module Fuzz.Action.Program exposing (defaultStyles, program)

import Color
import Element exposing (Element)
import Element.Attributes exposing (..)
import Fuzz.Action.Log exposing (Log)
import Fuzz.Action.Task exposing (..)
import Html exposing (..)
import Random.Pcg
import Style
import Style.Color exposing (..)
import Task exposing (Task)


type alias Model test =
    { runs : List (Log test)
    }


initialModel : Model test
initialModel =
    { runs = []
    }


type Msg test
    = RunComplete Int ( Log test, Random.Pcg.Seed )


update : Config real test -> Msg test -> Model test -> ( Model test, Cmd (Msg test) )
update config msg model =
    case msg of
        RunComplete remaining ( log, newSeed ) ->
            ( { model | runs = log :: model.runs }
            , runNext config remaining newSeed
            )


runNext : Config real test -> Int -> Random.Pcg.Seed -> Cmd (Msg test)
runNext config remaining seed =
    if remaining <= 0 then
        Cmd.none
    else
        go config seed
            |> Task.perform (RunComplete (remaining - 1))


go : Config real test -> Random.Pcg.Seed -> Task Never ( Log test, Random.Pcg.Seed )
go config =
    Fuzz.Action.Task.test config.real config.test config.actions
        >> (\( task, newSeed ) -> task |> Task.map (flip (,) newSeed))


type alias Config real test =
    { seed : Int
    , fuzz : Int
    , real : real
    , test : test
    , actions : List (Action real test)
    }


program : Config real test -> Program Never (Model test) (Msg test)
program config =
    Html.program
        { init =
            ( initialModel
            , go config
                (Random.Pcg.initialSeed config.seed)
                |> Task.perform (RunComplete config.fuzz)
            )
        , update = update config
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
