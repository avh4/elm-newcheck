module Fuzz.Action.Log exposing (Log, empty)

{-| -}


{-| A log of the state transitions that occurred during a test run.
Use for display if the test fails.
-}
type alias Log test =
    { init : test
    , steps : List ( String, test )
    , failure : Maybe ( String, String )
    }


empty : test -> Log test
empty init =
    { init = init
    , steps = []
    , failure = Nothing
    }
