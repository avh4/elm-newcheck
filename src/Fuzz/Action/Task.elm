module Fuzz.Action.Task exposing (Action, modify1, readAndModify0, test)

import Fuzz exposing (Fuzzer)
import Fuzz.Action.Log as Log exposing (Log)
import Random.Pcg
import Task exposing (Task)
import Test.Runner


type Action real test
    = Action (Fuzzer (ActionDetails real test))


type alias ActionDetails real test =
    { name : String
    , pre : test -> Bool
    , go : real -> test -> Task String ( real, test )
    }


modify1 :
    { name : String
    , pre : test -> Bool
    , action : arg -> real -> Task Never real
    , arg : Fuzzer arg
    , test : arg -> test -> Result String test
    }
    -> Action real test
modify1 config =
    let
        a arg =
            { name = config.name ++ " " ++ toString arg
            , pre = config.pre
            , go =
                \real testModel ->
                    case config.test arg testModel of
                        Err message ->
                            Task.fail message

                        Ok newTestModel ->
                            Task.map (flip (,) newTestModel) (config.action arg real)
                                |> Task.mapError never
            }
    in
    Fuzz.map a config.arg
        |> Action


readAndModify0 :
    { name : String
    , pre : test -> Bool
    , action : real -> Task Never ( result, real )
    , test : test -> ( result, test )
    }
    -> Action real test
readAndModify0 config =
    { name = config.name
    , pre = config.pre
    , go =
        \real testModel ->
            config.action real
                |> Task.mapError never
                |> Task.andThen
                    (\( actual, newReal ) ->
                        let
                            ( expected, newTest ) =
                                config.test testModel
                        in
                        if actual == expected then
                            Task.succeed ( newReal, newTest )
                        else
                            Task.fail <| "expected " ++ toString expected ++ ", but got: " ++ toString actual
                    )
    }
        |> Fuzz.constant
        |> Action


type alias StepValue real test =
    ( real, test, Log test )


run :
    ActionDetails real test
    -> StepValue real test
    -> Task ( String, String, Log test ) (StepValue real test)
run action previousResult =
    case previousResult of
        ( real, test, log ) ->
            -- TODO: check precondition
            action.go real test
                |> Task.mapError (\reason -> ( action.name, reason, log ))
                |> Task.map
                    (\( newReal, newTest ) ->
                        ( newReal
                        , newTest
                        , { log | steps = ( action.name, newTest ) :: log.steps }
                        )
                    )


test : real -> test -> List (Action real test) -> Random.Pcg.Seed -> ( Task Never (Log test), Random.Pcg.Seed )
test initialReal initialTestModel actions seed =
    -- TODO: use Random.Seed
    let
        gen =
            actions
                |> List.map (\(Action f) -> f)
                |> Fuzz.oneOf

        fuzzGen =
            Test.Runner.fuzz (Fuzz.list gen)

        ( ( first, shrink ), newSeed ) =
            Random.Pcg.step fuzzGen seed

        runOne : List (ActionDetails real test) -> Task Never (Result ( String, String, Log test ) (Log test))
        runOne actionDetails =
            List.foldl
                (\a prev -> prev |> Task.andThen (run a))
                (Task.succeed ( initialReal, initialTestModel, Log.empty initialTestModel ))
                actionDetails
                |> Task.map (\( _, _, log ) -> Ok log)
                |> Task.onError (Err >> Task.succeed)

        runShrink shrink result =
            case result of
                Err ( name, message, log ) ->
                    let
                        failureLog =
                            { log
                                | failure = Just ( name, message )
                            }
                    in
                    case Test.Runner.shrink False shrink of
                        Nothing ->
                            Task.succeed failureLog

                        Just ( next, nextShrink ) ->
                            runOne next
                                -- |> Task.map (Debug.log "shrink A")
                                |> Task.map (Result.map (always failureLog))
                                |> Task.andThen (runShrink nextShrink)

                Ok originalLog ->
                    case Test.Runner.shrink True shrink of
                        Nothing ->
                            Task.succeed originalLog

                        Just ( next, nextShrink ) ->
                            runOne next
                                -- |> Task.map (Debug.log "shrink B")
                                |> Task.map (Result.map (always originalLog))
                                |> Task.andThen (runShrink nextShrink)
    in
    runOne first
        -- |> Task.map (Debug.log "original")
        |> Task.andThen (runShrink shrink)
        |> flip (,) newSeed
