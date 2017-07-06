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
                , action = CircularBuffer.get >> Task.succeed
                , test =
                    \model ->
                        case model of
                            [] ->
                                PreconditionFailed "buffer is empty"

                            first :: rest ->
                                Check <|
                                    \actual ->
                                        if actual == Just first then
                                            Ok rest
                                        else
                                            Err ""
                }
            , modify1
                { name = "put"
                , action = CircularBuffer.put >>> Task.succeed
                , arg = Fuzz.int
                , test = \a t -> Ok (Ok (t ++ [ a ]))
                }
            ]
        }


(>>>) : (b -> c -> d) -> (d -> x) -> (b -> c -> x)
(>>>) base f b c =
    f (base b c)
