module Fuzz.Action exposing (Action, modify1, readAndModify0, test)

{-| This lets you define action specifications that compare
manipulations of a real data structure with a test model.

If you need to test a system that involves `Task`s, see `Fuzz.Action.Task`.


## Creating

@docs Action, modify1, readAndModify0


## Evaluating

@docs test

-}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Fuzz.Action.Log as Log exposing (Log)
import Test exposing (Test)


{-| A specification for an action to be tested with [`Fuzz.Action.test`](#test).
-}
type Action real test
    = Action (Fuzzer (ActionDetails real test))


type alias ActionDetails real test =
    { name : String
    , pre : test -> Bool
    , go : real -> test -> Result String ( real, test, Maybe String )
    }


{-| Creates a specification for a function of type `arg1 -> real -> real`.

That is, an action specification for an action
that takes one argument in addition to the primary data type,
and returns a new value of the primary data type.

-}
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
                        |> Result.map (\x -> ( config.action arg real, x, Nothing ))
            }
    in
    Fuzz.map a config.arg
        |> Action


{-| Creates a specification for a function of type `real -> (result, real)`.

That is, an action specification for an action
that takes no arguments other than the primary data type,
and returns both a result and a new value of the primary data type.

-}
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
                Ok ( newReal, newTest, Just (toString actual) )
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
                    Err { log | failure = Just { name = action.name, message = reason } }

                Ok ( newReal, newTest, output ) ->
                    Ok
                        ( newReal
                        , newTest
                        , { log | steps = { name = action.name, testModel = newTest, output = output } :: log.steps }
                        )


{-| Creates a fuzz test that ensures that randomly-chosen sequences of actions
produce the same results when applied to the real and test models.
-}
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
        showStep { name, testModel, output } =
            String.concat
                [ name
                , "  -->  "
                , toString testModel
                , output
                    |> Maybe.map (\x -> "  (" ++ x ++ ")")
                    |> Maybe.withDefault ""
                ]
    in
    case log.failure of
        Just { name, message } ->
            Expect.fail
                (showStep { name = "<init>", testModel = log.init, output = Nothing }
                    ++ "\n"
                    ++ String.join "\n" (List.map showStep log.steps)
                    ++ "\n"
                    ++ name
                    ++ "  ==>  FAILED\n\n    "
                    ++ message
                )

        Nothing ->
            Expect.pass
