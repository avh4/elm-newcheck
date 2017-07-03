module Go exposing (..)

import Array exposing (Array)
import Fuzz
import Fuzz.Action exposing (Action)
import Test exposing (..)


-- case actions of
--     a1 :: a2 :: _ ->
--         Ok ( initialReal, initialTestModel, Random.initialSeed 1 )
--             |> Fuzz.Action.run a2
--             |> Fuzz.Action.run a1
--             |> Fuzz.Action.run a2
--             |> Fuzz.Action.run a2
--             |> Fuzz.Action.run a1
--             |> Result.map (always ())
--
--     _ ->
--         Debug.crash "TODO: implement for variable number of actions"


runAll : Test
runAll =
    fuzz (Fuzz.list action) "runAll" <|
        \actions ->
            Fuzz.Action.test actions (emptyBuffer 3) []


action : Fuzz.Fuzzer (Action Buffer (List Int))
action =
    Fuzz.oneOf
        [ Fuzz.constant getSpec
        , putSpec
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
-- type TestResult
--     = GetResult ( Maybe Int, Buffer )
--     | PutResult Buffer


getSpec : Action Buffer (List Int)
getSpec =
    Fuzz.Action.readAndModify0
        { name = "get"
        , pre = \model -> not <| List.isEmpty model
        , action = get
        , test = \t -> ( List.head t, List.tail t |> Maybe.withDefault [] )
        }


putSpec : Fuzz.Fuzzer (Action Buffer (List Int))
putSpec =
    Fuzz.Action.modify1
        { name = "put"
        , pre = always True
        , action = put
        , arg = Fuzz.int
        , test = \a t -> Ok (t ++ [ a ])
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
