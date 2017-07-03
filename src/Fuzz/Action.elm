module Fuzz.Action exposing (Action, test)

import Expect exposing (Expectation)


type alias Action real test =
    { name : String
    , pre : real -> test -> Bool
    , go : real -> test -> ( real, test, Result String () )

    -- , realAction : real -> result
    -- , testAction : test -> test
    -- , post : result -> test -> Bool
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
run action previousResult =
    case previousResult of
        Err _ ->
            previousResult

        Ok ( real, test, log ) ->
            -- TODO: check precondition
            case action.go real test of
                ( _, _, Err reason ) ->
                    Err ( action.name, reason, log )

                ( newReal, newTest, Ok () ) ->
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
