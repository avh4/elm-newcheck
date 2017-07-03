module Go exposing (..)

import Array exposing (Array)
import Expect
import Fuzz
import Html
import Random
import Test exposing (..)


runTest : List (ActionSpec real test) -> real -> test -> Result String ()
runTest actions initialReal initialTestModel =
    List.foldl
        runAction
        (Ok ( initialReal, initialTestModel ))
        actions
        |> Result.map (always ())



-- case actions of
--     a1 :: a2 :: _ ->
--         Ok ( initialReal, initialTestModel, Random.initialSeed 1 )
--             |> runAction a2
--             |> runAction a1
--             |> runAction a2
--             |> runAction a2
--             |> runAction a1
--             |> Result.map (always ())
--
--     _ ->
--         Debug.crash "TODO: implement for variable number of actions"


runAction :
    ActionSpec real test
    -> Result String ( real, test )
    -> Result String ( real, test )
runAction action previousResult =
    case previousResult of
        Err _ ->
            previousResult

        Ok ( real, test ) ->
            let
                _ =
                    Debug.log "Running action" action.name
            in
            -- TODO: check precondition
            case action.go real test of
                ( _, _, Err reason ) ->
                    Err ("Post condition failed: " ++ action.name ++ "\n" ++ reason)

                ( newReal, newTest, Ok () ) ->
                    Ok ( newReal, newTest )
                        |> Debug.log "done"


runAll : Test
runAll =
    fuzz (Fuzz.list action) "runAll" <|
        \actions ->
            runTest actions (emptyBuffer 3) []
                |> Expect.equal (Ok ())


action : Fuzz.Fuzzer (ActionSpec Buffer (List Int))
action =
    Fuzz.oneOf
        [ Fuzz.constant getSpec
        , Fuzz.map putSpec Fuzz.int
        ]



-- runTest
--     [ putSpec 890
--     , getSpec
--     , putSpec 111
--     , putSpec 222
--     , getSpec
--     ]
--     (emptyBuffer 3)
--     []
--     |> toString
--     |> Html.text


type alias ActionSpec real test =
    { name : String
    , pre : real -> test -> Bool
    , go : real -> test -> ( real, test, Result String () )

    -- , realAction : real -> result
    -- , testAction : test -> test
    -- , post : result -> test -> Bool
    }



-- type TestResult
--     = GetResult ( Maybe Int, Buffer )
--     | PutResult Buffer


getSpec : ActionSpec Buffer (List Int)
getSpec =
    { name = "get"
    , pre = \real model -> not <| List.isEmpty model

    -- , realAction = \buffer -> get buffer
    -- , testAction = \testModel -> List.tail testModel
    -- , post =
    --     \( getValue, newBuffer ) testModel ->
    --         List.head testModel == getValue
    , go =
        \buffer testModel ->
            let
                result =
                    get buffer

                ( getValue, newBuffer ) =
                    result
            in
            ( newBuffer
            , List.tail testModel |> Maybe.withDefault []
            , if List.head testModel == getValue then
                Ok ()
              else
                Err <| "expected " ++ toString (List.head testModel) ++ ", but got: " ++ toString getValue
            )
    }


putSpec : Int -> ActionSpec Buffer (List Int)
putSpec arg =
    { name = "put " ++ toString arg
    , pre = \buffer testModel -> True
    , go =
        \buffer testModel ->
            let
                -- ( arg, newSeed ) =
                --     Random.step (Random.int Random.minInt Random.maxInt) seed
                result =
                    put arg buffer
            in
            ( result
            , testModel ++ [ arg ]
            , Ok ()
            )
    }


type Buffer
    = Buffer
        { data : Array Int
        , next : Int
        , last : Int
        }


emptyBuffer : Int -> Buffer
emptyBuffer capacity =
    Buffer
        { data = Array.repeat capacity 0
        , next = 0
        , last = 0
        }


get : Buffer -> ( Maybe Int, Buffer )
get (Buffer buffer) =
    if buffer.last == buffer.next then
        ( Nothing, Buffer buffer )
    else
        ( Array.get buffer.last buffer.data
        , Buffer { buffer | last = buffer.last + 1 }
        )


put : Int -> Buffer -> Buffer
put value (Buffer buffer) =
    Buffer
        { buffer
            | data = Array.set buffer.next value buffer.data
            , next = buffer.next + 1
        }
