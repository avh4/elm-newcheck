module Fuzz.Action.Task exposing (Action, modify1, readAndModify0, readAndModify1, readAndModify2, test)

{-| This lets you define action specifications that compare the result of
`Tasks` with a test model.

**NOTE**: You probably want to use `Fuzz.Action.Program.program`
instead of using this module directly if you want the
easiest way to evaluate Task-based action specifications.


## Creating

@docs Action
@docs modify1
@docs readAndModify0, readAndModify1, readAndModify2


## Evaluating

@docs test

-}

import Fuzz exposing (Fuzzer)
import Fuzz.Action.Log as Log exposing (Log)
import Random.Pcg
import Task exposing (Task)
import Test.Runner


{-| A specification for a task-based action to be tested with [`Fuzz.Action.Task.test`](#test).
-}
type Action real test
    = Action (Fuzzer (ActionDetails real test))


type alias ActionDetails real test =
    { name : String
    , pre : test -> Result String ()
    , go : real -> test -> Task String ( real, test, Maybe String )
    }


{-| Creates a specification for a function of type `arg1 -> real -> Task Never real`.

That is, a task-based action specification for an action
that takes one argument in addition to the primary data type,
and returns a new value of the primary data type.

-}
modify1 :
    { name : String
    , pre : arg -> test -> Result String ()
    , action : arg -> real -> Task Never real
    , arg : Fuzzer arg
    , test : arg -> test -> Result String test
    }
    -> Action real test
modify1 config =
    let
        a arg =
            { name = config.name ++ " " ++ toString arg
            , pre = config.pre arg
            , go =
                \real testModel ->
                    case config.test arg testModel of
                        Err message ->
                            Task.fail message

                        Ok newTestModel ->
                            config.action arg real
                                |> Task.map (\real -> ( real, newTestModel, Nothing ))
                                |> Task.mapError never
            }
    in
    Fuzz.map a config.arg
        |> Action


{-| Creates a specification for a function of type `real -> Task Never (result, real)`.

That is, a task-based action specification for an action
that takes no arguments other than the primary data type,
and returns both a result and a new value of the primary data type.

-}
readAndModify0 :
    { name : String
    , pre : test -> Result String ()
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
                            Task.succeed ( newReal, newTest, Just <| toString actual )
                        else
                            Task.fail <| "expected " ++ toString expected ++ ", but got: " ++ toString actual
                    )
    }
        |> Fuzz.constant
        |> Action


{-| Creates a specification for a function of type `arg1 -> real -> Task Never (result, real)`.

That is, a task-based action specification for an action
that takes one argument in addition to the primary data type,
and returns both a result and a new value of the primary data type.

-}
readAndModify1 :
    { name : String
    , pre : arg -> test -> Result String ()
    , arg : Fuzzer arg
    , action : arg -> real -> Task Never ( result, real )
    , test : arg -> test -> ( result, test )
    }
    -> Action real test
readAndModify1 config =
    let
        a arg =
            { name = config.name ++ " " ++ toString arg
            , pre = config.pre arg
            , go =
                \real testModel ->
                    config.action arg real
                        |> Task.mapError never
                        |> Task.andThen
                            (\( actual, newReal ) ->
                                let
                                    ( expected, newTest ) =
                                        config.test arg testModel
                                in
                                if actual == expected then
                                    Task.succeed ( newReal, newTest, Just <| toString actual )
                                else
                                    Task.fail <| "expected " ++ toString expected ++ ", but got: " ++ toString actual
                            )
            }
    in
    Fuzz.map a config.arg
        |> Action


{-| Creates a specification for a function of type `arg1 -> arg2 -> real -> Task Never (result, real)`.

That is, a task-based action specification for an action
that takes two arguments in addition to the primary data type,
and returns both a result and a new value of the primary data type.

-}
readAndModify2 :
    { name : String
    , pre : arg1 -> arg2 -> test -> Result String ()
    , arg1 : Fuzzer arg1
    , arg2 : Fuzzer arg2
    , action : arg1 -> arg2 -> real -> Task Never ( result, real )
    , test : arg1 -> arg2 -> test -> ( result, test )
    }
    -> Action real test
readAndModify2 config =
    let
        a arg1 arg2 =
            { name =
                String.join " "
                    [ config.name
                    , toString arg1
                    , toString arg2
                    ]
            , pre = config.pre arg1 arg2
            , go =
                \real testModel ->
                    config.action arg1 arg2 real
                        |> Task.mapError never
                        |> Task.andThen
                            (\( actual, newReal ) ->
                                let
                                    ( expected, newTest ) =
                                        config.test arg1 arg2 testModel
                                in
                                if actual == expected then
                                    Task.succeed ( newReal, newTest, Just <| toString actual )
                                else
                                    Task.fail <| "expected " ++ toString expected ++ ", but got: " ++ toString actual
                            )
            }
    in
    Fuzz.map2 a config.arg1 config.arg2
        |> Action


type alias StepValue real test =
    ( real, test, Log test )


run :
    ActionDetails real test
    -> StepValue real test
    -> Task (Log test) (StepValue real test)
run action previousResult =
    case previousResult of
        ( real, test, log ) ->
            case action.pre test of
                Ok () ->
                    action.go real test
                        |> Task.mapError (\reason -> { log | failure = Just { name = action.name, message = reason } })
                        |> Task.map
                            (\( newReal, newTest, output ) ->
                                ( newReal
                                , newTest
                                , { log | steps = { name = action.name, output = output, testModel = newTest } :: log.steps }
                                )
                            )

                Err reason ->
                    Task.succeed
                        ( real
                        , test
                        , { log
                            | steps =
                                { name = action.name ++ " (skipped; precondition failed; " ++ reason ++ ")"
                                , output = Nothing
                                , testModel = test
                                }
                                    :: log.steps
                          }
                        )


{-| Execute a single fuzz test iteration. Returns a task that does the following:

  - Generates a random sequence of actions from the provided list of action specifications
  - Evaluates each action, asserting that the results from the test model match the real results
  - If the evaluation fails:
      - Tries to shrink the sequence to find the minimal failing example
      - Returns the test log of the minimal failing example
        (with `log.failure = Just (<last action>, <assertion failure>)`)
  - If the evaluation succeeds:
      - Returns the test log of the evaluation (with `log.failure = Nothing`)

-}
test : Task String real -> test -> List (Action real test) -> Random.Pcg.Seed -> ( Task Never (Log test), Random.Pcg.Seed )
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

        runOne : List (ActionDetails real test) -> Task Never (Result (Log test) (Log test))
        runOne actionDetails =
            List.foldl
                (\a prev -> prev |> Task.andThen (run a))
                (initialReal
                    |> Task.map (\real -> ( real, initialTestModel, Log.empty initialTestModel ))
                    |> Task.mapError
                        (\reason ->
                            { init = initialTestModel
                            , steps = []
                            , failure = Just { name = "<init>", message = reason }
                            }
                        )
                )
                actionDetails
                |> Task.map (\( _, _, log ) -> Ok log)
                |> Task.onError (Err >> Task.succeed)

        runShrink shrink result =
            case result of
                Err failureLog ->
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
