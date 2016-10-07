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
    }

init : State
init =
    { questions = Result.Err Loading
    , quizzes = Result.Err Loading
    }

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
        if index == question.correct_answer
        then H.div [ A.style [ ( "text-decoration", "underline" )] ] [ H.text answer ]
        else H.div [] [ H.text answer ]
    in
        H.div []
            [ H.h2 [] [ H.text question.question ]
            , H.div [] ( List.indexedMap viewAnswer question.answers )
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
        H.div []
            [ H.h1 [] [ H.text quiz.title ]
            , H.div [] questions'
            ]

view' : State -> H.Html Message
view' state =
    case Result.map2 (,) state.questions state.quizzes of
        Result.Err Loading ->
            H.text "loading..."

        Result.Err ( Issue ( Http.UnexpectedPayload message )) ->
            H.text message

        Result.Err ( Issue error ) ->
            H.text "something went wrong"

        Result.Ok ( questions, quizzes ) ->
            H.div [] ( List.map ( viewQuiz questions ) quizzes )


view : State -> H.Html Message
view state =
    H.div []
        [ H.node "style" [] [ H.text "@import url(\"/style.css\")" ]
        , view' state
        ]

main = H.program
    { init = init ! [ getQuizzes, getQuestions ]
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }
