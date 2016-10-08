import Http
import Task
import String
import Navigation
import UrlParser as P exposing ((</>))
import Html as H
import Html.Attributes as A
import Html.Events as E
import Html.App as H
import Json.Decode as Decoder exposing ((:=))
import Json.Encode as Encoder

import Quiz
import Question
import Submission

type Status
    = Loading
    | Issue Http.Error

type Message
    = Sync
    | Submit
    | Questions ( Result Status ( List Question.Question ))
    | Quizzes ( Result Status ( List Quiz.Quiz ))
    | Grade ( Result Status Submission.Response )

encodeJSON encoder data =
    encoder data
    |> Encoder.encode 0
    |> Http.string

getQuizzes : Cmd Message
getQuizzes =
    Http.get
        ( "quizzes" := Decoder.list Quiz.decoder )
        "http://localhost:3000/quizzes"
        |> Task.perform
            ( Quizzes << Result.Err << Issue )
            ( Quizzes << Result.Ok )

getQuestions : Cmd Message
getQuestions =
    Http.get
        ( "questions" := Decoder.list Question.decoder )
        "http://localhost:3000/questions"
        |> Task.perform
            ( Questions << Result.Err << Issue )
            ( Questions << Result.Ok )

postSubmission : Submission.Answer -> Cmd Message
postSubmission submission =
    Http.post
        Submission.responseDecoder
        "http://localhost:3000/grade"
        ( encodeJSON Submission.encodeAnswer submission )
        |> Task.perform
            ( Grade << Result.Err << Issue )
            ( Grade << Result.Ok )

type alias State =
    { questions : Result Status ( List Question.Question )
    , quizzes : Result Status ( List Quiz.Quiz )
    , grade : Result Status Submission.Response
    , stage : Stage
    }

init : Result String Stage -> ( State, Cmd Message )
init data =
    { questions = Result.Err Loading
    , quizzes = Result.Err Loading
    , grade = Result.Err Loading
    , stage = getStage initStage data
    } ! [ getQuizzes, getQuestions ]

update : Message -> State -> ( State, Cmd Message )
update msg state =
    case msg of
        Sync ->
            state ! [ getQuizzes, getQuestions ]

        Questions questions ->
            { state | questions = questions } ! []

        Quizzes quizzes ->
            { state | quizzes = quizzes } ! []

        Grade grade ->
            { state | grade = grade } ! []

        Submit ->
            let
                {-
                submission =
                    Submission.Submission
                        1
                        [ Submission.Answer 1 2
                        , Submission.Answer 2 3
                        , Submission.Answer 3 2
                        ]
                        -}
                submission = Submission.Answer 1 2
            in
                state ! [ postSubmission submission ]



viewQuestion : Question.Question -> H.Html Message
viewQuestion question =
    let viewAnswer index answer =
        H.li
            [ A.classList
                [ ("answer-item", True)
                , ("answer-item-correct", index == question.correct_answer)
                ]
            ]
            [ H.text answer ]
    in
        H.div
            [ A.class "question-content" ]
            [ H.h2 [] [ H.text question.question ]
            , H.ul
                [ A.class "answer-list" ]
                ( List.indexedMap viewAnswer question.answers )
            ]

viewQuiz : List Question.Question -> Quiz.Quiz -> H.Html Message
viewQuiz questions quiz =
    let
        questionInQuiz question =
            List.member question.id quiz.question_ids

        questions' =
            questions
            |> List.filter questionInQuiz
            |> List.map viewQuestion
    in
        H.div
            [ A.class "quiz-content" ]
            [ H.div []
                [ H.a
                    [ A.href "/" ]
                    [ H.text "back" ]
                ]
            , H.h1 [] [ H.text quiz.title ]
            , H.div [] questions'
            ]

viewQuizSummary : List Quiz.Quiz -> H.Html Message
viewQuizSummary quizzes =
    let viewQuiz' quiz =
        H.li
            [ A.class "quiz-list-item" ]
            [ H.a
                [ A.href <| "/quiz/" ++ (toString quiz.id) ]
                [ H.text quiz.title ]
            ]
    in
        H.ul
            [ A.class "quiz-list" ]
            ( List.map viewQuiz' quizzes )

viewNoQuiz : H.Html Message
viewNoQuiz =
    H.div
        []
        [ H.div []
            [ H.a
                [ A.href "/" ]
                [ H.text "back" ]
            ]
        , H.h2 [] [ H.text "That quiz doesn't exist!" ]
        ]

view' : State -> H.Html Message
view' state =
    case Result.map2 (,) state.questions state.quizzes of
        Result.Err Loading ->
            H.text "Loading..."

        Result.Err ( Issue ( Http.UnexpectedPayload message )) ->
            H.text message

        Result.Err ( Issue Http.Timeout ) ->
            H.text "Something seems wrong with your connection, have a look and try again"

        Result.Err ( Issue Http.NetworkError ) ->
            H.text "Something seems wrong with the connection, have a look and try again"

        Result.Err ( Issue ( Http.BadResponse code msg )) ->
            H.text "Ugh, something went wrong on our end... Sorry about that :|"

        Result.Ok ( questions, quizzes ) ->
            case state.stage of
                List ->
                    H.div []
                    [ H.button
                        [ E.onClick Submit ]
                        [ H.text "submit" ]
                    , H.div [] [ H.text ( toString state.grade ) ]
                    , viewQuizSummary quizzes
                    ]

                ViewQuiz id ->
                    List.filter (\q -> q.id == id) quizzes
                    |> List.head
                    |> Maybe.map ( viewQuiz questions )
                    |> Maybe.withDefault viewNoQuiz


view : State -> H.Html Message
view state =
    H.div
        [ A.class "page" ]
        [ H.node "style" [] [ H.text "@import url(\"/style.css\")" ]
        , view' state
        ]

-- The current page
type Stage
    = List
    | ViewQuiz Int

initStage = List

-- Try to parse a url location into a Stage.
-- Our program calls this every time the URL changes
parser : Navigation.Location -> Result String Stage
parser location =
    let
        parser' =
            P.oneOf
            [ P.format ViewQuiz ( P.s "quiz" </> P.int )
            , P.format List ( P.s "quiz" )
            , P.format List ( P.s "" )
            ]
    in
        P.parse
            identity -- Our parser gives us a Stage, no post-processing needed
            parser'
            ( String.dropLeft 1 location.pathname ) -- Drop the /

-- Just show the last good page if something goes wrong when parsing URL
getStage = Result.withDefault

-- Update our state with the result of our parser whenever URL changes
urlUpdate data state =
    { state | stage = getStage state.stage data } ! []

main = Navigation.program
    ( Navigation.makeParser parser )
    { init = init
    , update = update
    , view = view
    , urlUpdate = urlUpdate
    , subscriptions = always Sub.none
    }
