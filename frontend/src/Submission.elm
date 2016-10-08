module Submission exposing (..)

import Json.Encode as Encoder
import Json.Decode as Decoder exposing ((:=))

import Question

type Status
    = Empty
    | Chosen Int
    | Submitted Response

status_correct status =
    case status of
        Submitted {correct} -> Just correct

        otherwise -> Nothing

type alias Answer =
    { questionID : Int
    , answer : Int
    }

type alias Submission =
    { quizID : Int
    , answers : List Answer
    }

type alias Response =
    { question : Question.Question
    , correct : Bool
    }

type alias Responses =
    List Response

encodeAnswer : Answer -> Encoder.Value
encodeAnswer {questionID,answer} =
    Encoder.object
    [ ( "question_id", Encoder.int questionID )
    , ( "answer", Encoder.int answer )
    ]

encode : Submission -> Encoder.Value
encode {quizID,answers} =
    let encodedAnswers = List.map encodeAnswer answers
    in
        Encoder.object
        [ ( "quiz_id", Encoder.int quizID )
        , ( "answers", Encoder.list encodedAnswers )
        ]

responseDecoder : Decoder.Decoder Response
responseDecoder =
    Decoder.object2 Response
    ( "question" := Question.decoder )
    ( "correct" := Decoder.bool )

responsesDecoder : Decoder.Decoder Responses
responsesDecoder =
    Decoder.list responseDecoder

