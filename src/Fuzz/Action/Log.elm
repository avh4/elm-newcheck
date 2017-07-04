module Fuzz.Action.Log exposing (Log, empty)

{-|

@docs Log, empty

-}


{-| A log of the state transitions that occurred during a test run.
Use for display if the test fails.

  - `init`: The initial test model
  - `steps`: The list of actions performed
      - `name`: The name of the action
      - `output`: The
      - `testModel`: The test model after applying the action
  - `failure`: One of
      - `Nothing`: The test execution completed successfully
      - `Just { name, message }`: The name of the action that failed, and the failure message

-}
type alias Log test =
    { init : test
    , steps : List { name : String, output : Maybe String, testModel : test }
    , failure : Maybe { name : String, message : String }
    }


{-| An empty test log
-}
empty : test -> Log test
empty init =
    { init = init
    , steps = []
    , failure = Nothing
    }
