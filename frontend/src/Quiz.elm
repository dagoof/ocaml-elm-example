module Quiz exposing (..)

import Json.Decode as Decoder exposing ((:=))

type alias Quiz =
    { id : Int
    , title : String
    , question_ids : List Int
    }

decoder : Decoder.Decoder Quiz
decoder =
    Decoder.object3 Quiz
    ( "id" := Decoder.int )
    ( "title" := Decoder.string )
    ( "question_ids" := Decoder.list Decoder.int )

