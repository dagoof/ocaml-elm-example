import Http
import Dict
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


onPreventDefaultClick : msg -> H.Attribute msg
onPreventDefaultClick msg =
    let
        options =
            E.defaultOptions

        decoder =
            ( "metaKey" := Decoder.bool ) `Decoder.andThen` (\metaDown ->
                if not metaDown
                then Decoder.succeed msg
                else Decoder.fail "Meta key down, let default happen"
            )
    in
        E.onWithOptions "click"
            { options | preventDefault = True }
            decoder

-- Take us to a URL through elm's Navigation system.
goto url =
    [ A.href url
    , onPreventDefaultClick ( Page url )
    ]

-- Helper to avoid this ugly string concat constantly
gotoQuiz id =
    goto ( "/quiz/" ++ ( toString id ))

-- Let HTTP Requests also represent a loading state before we get the result
type Status
    = Loading
    | Issue Http.Error

type Message
    = Sync
    | Submit Int
    | SubmitQuiz ( List Int )
    | SelectAnswer Int Int
    | Page String
    | Questions ( Result Status ( List Question.Question ))
    | Quizzes ( Result Status ( List Quiz.Quiz ))
    | Grade Int ( Result Status Submission.Response )

-- Helper to create Body suitable to POST
encodeJSON : ( a -> Encoder.Value ) -> a -> Http.Body
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

-- Post a single Answer and get back its corresponding Response
postSubmission : Submission.Answer -> Cmd Message
postSubmission submission =
    Http.post
        Submission.responseDecoder
        "http://localhost:3000/grade"
        ( encodeJSON Submission.encodeAnswer submission )
        |> Task.perform
            ( Grade submission.questionID << Result.Err << Issue )
            ( Grade submission.questionID << Result.Ok )

{-
we bootstrap our app with questions and quizzes and use them for everything

stage is the page the user is looking at right now

answers is a dump where we track what the current state of all questions
this includes:

    which answer is selected
    has the question been subimitted
    was that submission successful
    was the answer we got back correct
-}
type alias State =
    { questions : Result Status ( List Question.Question )
    , quizzes : Result Status ( List Quiz.Quiz )
    , answers : Dict.Dict Int Submission.Tracker
    , stage : Stage
    }

init : Result String Stage -> ( State, Cmd Message )
init data =
    { questions = Result.Err Loading
    , quizzes = Result.Err Loading
    , answers = Dict.empty
    , stage = getStage initStage data
    } ! [ getQuizzes, getQuestions ]

-- If we have an answer for a question, create a command to submit it
submitQuestion : Int -> Dict.Dict Int Submission.Tracker -> Cmd Message
submitQuestion questionID answers =
    Dict.get questionID answers
    `Maybe.andThen` Submission.trackerSelected
    |> Maybe.map ( Submission.Answer questionID )
    |> Maybe.map postSubmission
    |> Maybe.withDefault Cmd.none

-- Create a new state and a command to run as the result of a message
update : Message -> State -> ( State, Cmd Message )
update msg state =
    case msg of
        -- Retry initializing our questions and quizzes.
        -- They should already be here from init
        Sync ->
            state ! [ getQuizzes, getQuestions ]

        Page s ->
            state ! [ Navigation.newUrl s ]

        -- Handle question response
        Questions questions ->
            { state | questions = questions } ! []

        -- Handle quiz response
        Quizzes quizzes ->
            { state | quizzes = quizzes } ! []

        -- Mark off a grade after as a result of submitting an answer
        Grade questionID grade ->
            let
                status =
                    grade
                    |> Result.toMaybe
                    |> Maybe.map Submission.Submitted
                    |> Maybe.withDefault Submission.Failed

                answers =
                    Dict.update
                        questionID
                        ( Maybe.map ( Submission.status status ))
                        state.answers
            in
               { state | answers = answers } ! []

        -- Submit a single question
        Submit questionID ->
            state ! [ submitQuestion questionID state.answers ]

        -- Submit an entire quiz worth of questions
        SubmitQuiz questionIDs ->
            state ! ( List.map ( flip submitQuestion state.answers ) questionIDs )

        -- Select a new answer and drop any associated *success* *failure* *response* data
        SelectAnswer questionID index ->
            let
                select tracker =
                    tracker
                    |> Maybe.withDefault Submission.trackerInit
                    |> Submission.select index
                    |> Submission.status Submission.Pending
                    |> Just

                answers =
                    Dict.update questionID select state.answers
            in
               { state | answers = answers } ! []


viewQuestion : Submission.Tracker -> Question.Question -> H.Html Message
viewQuestion tracker question =
    let
        -- Is our answer graded as correct?
        statusCorrect =
            Submission.trackerCorrect tracker

        viewAnswer index answer =
            H.li
                [ A.classList
                    [ ("answer-item", True)
                    , ("answer-item-selected", Submission.selected index tracker )
                    , ("answer-item-correct", index == question.correct_answer)
                    ]
                , E.onClick ( SelectAnswer question.id index )
                ]
                [ H.text answer ]

        -- Don't allow clicking when we know the answer is correct
        viewReadOnlyAnswer index answer =
            H.li
                [ A.classList
                    [ ("answer-item", True)
                    , ("answer-item-selected", Submission.selected index tracker )
                    , ("answer-item-correct", index == question.correct_answer)
                    ]
                ]
                [ H.text answer ]

        answerView =
            if Maybe.withDefault False statusCorrect
               then viewReadOnlyAnswer
               else viewAnswer
    in
        H.div
            [ A.classList
                [ ("question-content", True)
                , ("question-content-correct", Just True == statusCorrect)
                , ("question-content-incorrect", Just False == statusCorrect)
                ]
            ]
            [ H.h2 [] [ H.text question.question ]
            , H.ul
                [ A.class "answer-list" ]
                ( List.indexedMap answerView question.answers )
            ]

viewQuiz
    : Dict.Dict Int Submission.Tracker
    -> List Question.Question
    -> Quiz.Quiz
    -> H.Html Message
viewQuiz answers questions quiz =
    let
        questionInQuiz question =
            List.member question.id quiz.question_ids

        -- The tracker for a given question
        tracker question =
            answers
            |> Dict.get question.id
            |> Maybe.withDefault Submission.trackerInit

        -- The subset of questions that are in this quiz
        questions' =
            List.filter questionInQuiz questions

        -- View for a single question
        questionView question =
            viewQuestion ( tracker question ) question

        -- have all the questions been answered?
        allAnswered =
            List.all ( tracker >> Submission.trackerHasSelection ) questions'

        -- have all the questions been answered and are also graded as correct?
        allCorrect =
            let
                correct = Maybe.withDefault False
            in
                List.all ( tracker >> Submission.trackerCorrect >> correct ) questions'
    in
        H.div
            [ A.class "quiz-content" ]
            [ H.div []
                [ H.a
                    ( goto "/" )
                    [ H.text "« back" ]
                ]
            , H.h1 [] [ H.text quiz.title ]
            , H.div [] ( List.map questionView questions' )
            , H.div
                [ A.class "quiz-controls" ]
                [ H.button
                    -- Only submit when all answered
                    [ E.onClick ( SubmitQuiz <| List.map .id questions' )
                    , A.disabled ( not allAnswered )
                    ]
                    [ H.text "Submit" ]
                , H.button
                    -- Only next when all correct
                    ( A.disabled ( not allCorrect ) :: gotoQuiz ( quiz.id + 1 ))
                    [ H.text "Next" ]
                ]
            ]

viewQuizSummary : List Quiz.Quiz -> H.Html Message
viewQuizSummary quizzes =
    let viewQuiz' quiz =
        H.li
            [ A.class "quiz-list-item" ]
            [ H.a
                ( gotoQuiz quiz.id )
                [ H.text quiz.title ]
            ]
    in
        H.ul
            [ A.class "quiz-list" ]
            [ H.h1 [] [ H.text "overview" ]
            , H.div [] ( List.map viewQuiz' quizzes )
            ]

viewNoQuiz : H.Html Message
viewNoQuiz =
    H.div
        []
        [ H.div []
            [ H.a
                ( goto "/" )
                [ H.text "« back" ]
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

        -- We need both questions and quizzes to do anything
        Result.Ok ( questions, quizzes ) ->
            case state.stage of
                List ->
                    viewQuizSummary quizzes

                ViewQuiz id ->
                    List.filter (\q -> q.id == id) quizzes
                    |> List.head
                    |> Maybe.map ( viewQuiz state.answers questions )
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
    let
        stage =
            getStage state.stage data
    in
        case stage of
            ViewQuiz id ->
                { state | stage = stage, answers = Dict.empty } ! []

            otherwise ->
                { state | stage = stage } ! []

main = Navigation.program
    ( Navigation.makeParser parser )
    { init = init
    , update = update
    , view = view
    , urlUpdate = urlUpdate
    , subscriptions = always Sub.none
    }
