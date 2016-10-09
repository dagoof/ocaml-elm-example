module Question exposing (..)

import Json.Decode as Decoder exposing ((:=))

-- Match the backend Question datatype
type alias Question =
    { id : Int
    , question : String
    , answers : List String
    , correct_answer : Int
    }

decoder : Decoder.Decoder Question
decoder =
    Decoder.object4 Question
    ( "id" := Decoder.int )
    ( "question" := Decoder.string )
    ( "answers" := Decoder.list Decoder.string )
    ( "correct_answer" := Decoder.int )

