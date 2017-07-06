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
        , real = Task.succeed <| CircularBuffer.empty 3
        , test = []
        , actions =
            [ readAndModify0
                { name = "get"
                , pre =
                    \model ->
                        if List.isEmpty model then
                            Err "buffer is empty"
                        else
                            Ok ()
                , action = CircularBuffer.get >> Task.succeed
                , test = \t -> Ok ( List.head t, List.tail t |> Maybe.withDefault [] )

                -- TODO: combine with the precondition so we can safely destructure
                }
            , modify1
                { name = "put"
                , pre = \_ _ -> Ok ()
                , action = CircularBuffer.put >>> Task.succeed
                , arg = Fuzz.int
                , test = \a t -> Ok (t ++ [ a ])
                }
            ]
        }


(>>>) : (b -> c -> d) -> (d -> x) -> (b -> c -> x)
(>>>) base f b c =
    f (base b c)
