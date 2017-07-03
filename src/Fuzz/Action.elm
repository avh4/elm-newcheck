module Fuzz.Action exposing (Action, modify1, readAndModify0, test)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)


type Action real test
    = Action
        { name : String
        , pre : test -> Bool
        , go : real -> test -> Result String ( real, test )
        }


modify1 :
    { name : String
    , pre : test -> Bool
    , action : arg -> real -> real
    , arg : Fuzzer arg
    , test : arg -> test -> Result String test
    }
    -> Fuzzer (Action real test)
modify1 config =
    let
        a arg =
            Action
                { name = config.name ++ " " ++ toString arg
                , pre = config.pre
                , go =
                    \real testModel ->
                        config.test arg testModel
                            |> Result.map ((,) (config.action arg real))
                }
    in
    Fuzz.map a config.arg


readAndModify0 :
    { name : String
    , pre : test -> Bool
    , action : real -> ( result, real )
    , test : test -> ( result, test )
    }
    -> Action real test
readAndModify0 config =
    Action
        { name = config.name
        , pre = config.pre
        , go =
            \real testModel ->
                let
                    ( actual, newReal ) =
                        config.action real

                    ( expected, newTest ) =
                        config.test testModel
                in
                if actual == expected then
                    Ok ( newReal, newTest )
                else
                    Err <| "expected " ++ toString expected ++ ", but got: " ++ toString actual
        }


type alias StepValue real test =
    ( real, test, Log test )


{-| A log of the state transitions that occurred during a test run.
Use for display if the test fails.
-}
type alias Log test =
    { init : test
    , steps : List ( String, test )
    }


run :
    Action real test
    -> Result ( String, String, Log test ) (StepValue real test)
    -> Result ( String, String, Log test ) (StepValue real test)
run (Action action) previousResult =
    case previousResult of
        Err _ ->
            previousResult

        Ok ( real, test, log ) ->
            -- TODO: check precondition
            case action.go real test of
                Err reason ->
                    Err ( action.name, reason, log )

                Ok ( newReal, newTest ) ->
                    Ok
                        ( newReal
                        , newTest
                        , { log | steps = ( action.name, newTest ) :: log.steps }
                        )


test : List (Action real test) -> real -> test -> Expectation
test actions initialReal initialTestModel =
    List.foldl
        run
        (Ok ( initialReal, initialTestModel, Log initialTestModel [] ))
        actions
        |> showResult


showResult : Result ( String, String, Log test ) (StepValue real test) -> Expectation
showResult result =
    let
        showStep ( step, output ) =
            step ++ "  -->  " ++ toString output
    in
    case result of
        Err ( last, message, log ) ->
            Expect.fail
                (showStep ( "<init>", log.init )
                    ++ "\n"
                    ++ String.join "\n" (List.map showStep log.steps)
                    ++ "\n"
                    ++ last
                    ++ "  ==>  FAILED\n\n    "
                    ++ message
                )

        Ok _ ->
            Expect.pass
