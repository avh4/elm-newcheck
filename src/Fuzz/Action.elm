module Fuzz.Action exposing (Action, test)


type alias Action real test =
    { name : String
    , pre : real -> test -> Bool
    , go : real -> test -> ( real, test, Result String () )

    -- , realAction : real -> result
    -- , testAction : test -> test
    -- , post : result -> test -> Bool
    }


run :
    Action real test
    -> Result String ( real, test )
    -> Result String ( real, test )
run action previousResult =
    case previousResult of
        Err _ ->
            previousResult

        Ok ( real, test ) ->
            -- TODO: check precondition
            case action.go real test of
                ( _, _, Err reason ) ->
                    Err ("Post condition failed: " ++ action.name ++ "\n" ++ reason)

                ( newReal, newTest, Ok () ) ->
                    Ok ( newReal, newTest )


test : List (Action real test) -> real -> test -> Result String ()
test actions initialReal initialTestModel =
    List.foldl
        run
        (Ok ( initialReal, initialTestModel ))
        actions
        |> Result.map (always ())
