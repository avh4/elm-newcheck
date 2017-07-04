module Fuzz.Action exposing (Action, modify1, readAndModify0, test)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Fuzz.Action.Log as Log exposing (Log)
import Test exposing (Test)


type Action real test
    = Action (Fuzzer (ActionDetails real test))


type alias ActionDetails real test =
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
    -> Action real test
modify1 config =
    let
        a arg =
            { name = config.name ++ " " ++ toString arg
            , pre = config.pre
            , go =
                \real testModel ->
                    config.test arg testModel
                        |> Result.map ((,) (config.action arg real))
            }
    in
    Fuzz.map a config.arg
        |> Action


readAndModify0 :
    { name : String
    , pre : test -> Bool
    , action : real -> ( result, real )
    , test : test -> ( result, test )
    }
    -> Action real test
readAndModify0 config =
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
        |> Fuzz.constant
        |> Action


type alias StepValue real test =
    ( real, test, Log test )


run :
    ActionDetails real test
    -> StepValue real test
    -> Result (Log test) (StepValue real test)
run action previousResult =
    case previousResult of
        ( real, test, log ) ->
            -- TODO: check precondition
            case action.go real test of
                Err reason ->
                    Err { log | failure = Just ( action.name, reason ) }

                Ok ( newReal, newTest ) ->
                    Ok
                        ( newReal
                        , newTest
                        , { log | steps = ( action.name, newTest ) :: log.steps }
                        )


test : String -> real -> test -> List (Action real test) -> Test
test name initialReal initialTestModel actions =
    let
        gen =
            actions
                |> List.map (\(Action f) -> f)
                |> Fuzz.oneOf
    in
    Test.fuzz (Fuzz.list gen) name <|
        \actionDetails ->
            List.foldl
                (\a r -> Result.andThen (run a) r)
                (Ok ( initialReal, initialTestModel, Log.empty initialTestModel ))
                actionDetails
                |> (\result ->
                        case result of
                            Ok ( _, _, log ) ->
                                log

                            Err log ->
                                log
                   )
                |> showResult


showResult : Log test -> Expectation
showResult log =
    let
        showStep ( step, output ) =
            step ++ "  -->  " ++ toString output
    in
    case log.failure of
        Just ( last, message ) ->
            Expect.fail
                (showStep ( "<init>", log.init )
                    ++ "\n"
                    ++ String.join "\n" (List.map showStep log.steps)
                    ++ "\n"
                    ++ last
                    ++ "  ==>  FAILED\n\n    "
                    ++ message
                )

        Nothing ->
            Expect.pass
