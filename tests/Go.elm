module Go exposing (..)

import Array exposing (Array)
import Fuzz
import Fuzz.Action exposing (Action)
import Test exposing (..)


runAll : Test
runAll =
    Fuzz.Action.test [ getSpec, putSpec ] (emptyBuffer 3) []


getSpec : Action Buffer (List Int)
getSpec =
    Fuzz.Action.readAndModify0
        { name = "get"
        , pre = \model -> not <| List.isEmpty model
        , action = get
        , test = \t -> ( List.head t, List.tail t |> Maybe.withDefault [] )

        -- TODO: combine with the precondition so we can safely destructure
        }


putSpec : Action Buffer (List Int)
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
