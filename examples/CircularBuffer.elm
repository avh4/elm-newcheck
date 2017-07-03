module CircularBuffer exposing (CircularBuffer, empty, get, put)

{-| This is a contrived example in Elm,
but is an example commonly used in quickcheck examples
for demonstrating quickcheck testing of state machines.

This module represents a FIFO with a maxiumum capacity.

-}

import Array exposing (Array)


type CircularBuffer
    = Buffer
        { data : Array Int
        , next : Int
        , last : Int
        }


empty : Int -> CircularBuffer
empty capacity =
    Buffer
        { data = Array.repeat capacity 0
        , next = 0
        , last = 0
        }


get : CircularBuffer -> ( Maybe Int, CircularBuffer )
get (Buffer buffer) =
    if buffer.last == buffer.next then
        ( Nothing, Buffer buffer )
    else
        ( Array.get buffer.last buffer.data
        , Buffer { buffer | last = buffer.last + 1 }
        )


put : Int -> CircularBuffer -> CircularBuffer
put value (Buffer buffer) =
    Buffer
        { buffer
            | data = Array.set buffer.next value buffer.data
            , next = buffer.next + 1
        }
