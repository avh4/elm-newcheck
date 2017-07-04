module TaskExample exposing (main)

import CircularBuffer
import Fuzz
import Fuzz.Action.Program
import Fuzz.Action.Task exposing (..)
import Task


main =
    Fuzz.Action.Program.program
        { seed = 0
        , fuzz = 100
        , real = CircularBuffer.empty 3
        , test = []
        , actions =
            [ readAndModify0
                { name = "get"
                , pre = \model -> not <| List.isEmpty model
                , action = CircularBuffer.get >> Task.succeed
                , test = \t -> ( List.head t, List.tail t |> Maybe.withDefault [] )

                -- TODO: combine with the precondition so we can safely destructure
                }
            , modify1
                { name = "put"
                , pre = always True
                , action = CircularBuffer.put >>> Task.succeed
                , arg = Fuzz.int
                , test = \a t -> Ok (t ++ [ a ])
                }
            ]
        }


(>>>) : (b -> c -> d) -> (d -> x) -> (b -> c -> x)
(>>>) base f b c =
    f (base b c)
