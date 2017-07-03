module CircularBufferSpec exposing (..)

import CircularBuffer exposing (CircularBuffer)
import Fuzz
import Fuzz.Action exposing (Action)
import Test exposing (..)


runAll : Test
runAll =
    Fuzz.Action.test "runAll"
        (CircularBuffer.empty 3)
        []
        [ getSpec, putSpec ]


getSpec : Action CircularBuffer (List Int)
getSpec =
    Fuzz.Action.readAndModify0
        { name = "get"
        , pre = \model -> not <| List.isEmpty model
        , action = CircularBuffer.get
        , test = \t -> ( List.head t, List.tail t |> Maybe.withDefault [] )

        -- TODO: combine with the precondition so we can safely destructure
        }


putSpec : Action CircularBuffer (List Int)
putSpec =
    Fuzz.Action.modify1
        { name = "put"
        , pre = always True
        , action = CircularBuffer.put
        , arg = Fuzz.int
        , test = \a t -> Ok (t ++ [ a ])
        }
