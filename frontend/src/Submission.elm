module Submission exposing (..)

import Json.Encode as Encoder
import Json.Decode as Decoder exposing ((:=))

import Question

-- Module Submission tracks the state of a question as its answer is submitted

-- A question can have succeeded, failed, or not have been submitted yet
type Status
    = Pending
    | Submitted Response
    | Failed

-- Which answer is selected? what is our status?
type alias Tracker =
    { selected : Maybe Int
    , status : Status
    }

-- Set the status of a tracker
status : Status -> Tracker -> Tracker
status s t =
    { t | status = s }

-- Set the selected answer of a tracker
select : Int -> Tracker -> Tracker
select index t =
    { t | selected = Just index }

-- Check that a tracker has a given answer selected
selected : Int -> Tracker -> Bool
selected index t =
    t.selected == Just index

-- Initialize an empty tracker
trackerInit : Tracker
trackerInit =
    { selected = Nothing
    , status = Pending
    }

-- Accessor for tracker.selected
trackerSelected : Tracker -> Maybe Int
trackerSelected =
    .selected

-- Does the tracker have any selected answer?
trackerHasSelection : Tracker -> Bool
trackerHasSelection tracker =
    tracker.selected
    |> Maybe.map ( always True )
    |> Maybe.withDefault False

-- Is the tracker submitted?
-- Does the response indicate that the answer was correct?
trackerCorrect : Tracker -> Maybe Bool
trackerCorrect tracker =
    case tracker.status of
        Submitted {correct} ->
            Just correct

        otherwise ->
            Nothing

-- Answer a single Question
type alias Answer =
    { questionID : Int
    , answer : Int
    }

-- Answer all the questions in a Quiz
type alias Submission =
    { quizID : Int
    , answers : List Answer
    }

-- The response for an Answer
type alias Response =
    { question : Question.Question
    , correct : Bool
    }

-- The response for a Submission
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

