import Http
import Task
import String
import Navigation
import UrlParser as P exposing ((</>))
import Html as H
import Html.Attributes as A
import Html.App as H
import Json.Decode as Decoder exposing ((:=))

type Status
    = Loading
    | Issue Http.Error

type Message
    = Sync
    | Questions ( Result Status ( List Question ))
    | Quizzes ( Result Status ( List Quiz ) )

type alias Question =
    { id : Int
    , question : String
    , answers : List String
    , correct_answer : Int
    }

type alias Quiz =
    { id : Int
    , title : String
    , question_ids : List Int
    }

questionDecoder : Decoder.Decoder Question
questionDecoder =
    Decoder.object4 Question
    ( "id" := Decoder.int )
    ( "question" := Decoder.string )
    ( "answers" := Decoder.list Decoder.string )
    ( "correct_answer" := Decoder.int )

quizDecoder : Decoder.Decoder Quiz
quizDecoder =
    Decoder.object3 Quiz
    ( "id" := Decoder.int )
    ( "title" := Decoder.string )
    ( "question_ids" := Decoder.list Decoder.int )

getQuizzes : Cmd Message
getQuizzes =
    Http.get
        ( "quizzes" := Decoder.list quizDecoder )
        "http://localhost:3000/quizzes"
        |> Task.perform
            ( Quizzes << Result.Err << Issue )
            ( Quizzes << Result.Ok )

getQuestions : Cmd Message
getQuestions =
    Http.get
        ( "questions" := Decoder.list questionDecoder )
        "http://localhost:3000/questions"
        |> Task.perform
            ( Questions << Result.Err << Issue )
            ( Questions << Result.Ok )


type alias State =
    { questions : Result Status ( List Question )
    , quizzes : Result Status ( List Quiz )
    , stage : Stage
    }

init : Result String Stage -> ( State, Cmd Message )
init data =
    { questions = Result.Err Loading
    , quizzes = Result.Err Loading
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


viewQuestion : Question -> H.Html Message
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

viewQuiz : List Question -> Quiz -> H.Html Message
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

viewQuizSummary : List Quiz -> H.Html Message
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
                    viewQuizSummary quizzes

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
