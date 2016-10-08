module Question = struct
    type t =
        { id : int
        ; question : string
        ; answers : string list
        ; correct_answer : int
        } [@@deriving yojson]

    let id_is ~id t =
        t.id = id

    let answer_is ~answer t =
        t.correct_answer = answer
end

module Quiz = struct
    type t = 
        { id : int
        ; title : string
        ; question_ids : int list
        } [@@deriving yojson]

    let id_is ~id t =
        t.id = id

    let questions ~questions t =
        let get_question id =
            List.find ( Question.id_is ~id ) questions
        in
        List.map get_question t.question_ids
end

module Questions = struct
    type t =
        { questions : Question.t list }
        [@@deriving yojson]

    let get id t =
        List.find ( Question.id_is ~id ) t.questions

    let questions t = t.questions

end

module Quizzes = struct
    type t =
        { quizzes : Quiz.t list }
        [@@deriving yojson]

    let get id t =
        List.find ( Quiz.id_is ~id ) t.quizzes

    let quizzes t = t.quizzes
end

module Submission = struct
    type t =
        { question_id : int
        ; answer : int
        } [@@deriving yojson]

    let answer t = t.answer

    let question_id t = t.question_id

    let question_id_is question_id t =
        question_id = t.question_id
end

module Submissions = struct
    type t =
        { quiz_id : int
        ; submissions : Submission.t list
        } [@@deriving yojson]

    let get_answer ~question_id t =
        Submission.answer @@
        List.find
            ( fun s -> question_id = Submission.question_id s )
            t.submissions
end

let fold_result fn items =
    let folder a acc =
        let open Rresult in
        acc >>= fun items -> fn a >>| fun item -> item :: items
    in
    List.fold_right folder items ( Result.Ok [] )

module Grading : sig
    type t
    type grade

    val grade_to_yojson : grade -> Yojson.Safe.json
    val grader_to_yojson : ( grade, string ) Result.result -> Yojson.Safe.json
    val graders_to_yojson : ( grade list, string ) Result.result -> Yojson.Safe.json

    val create : Quizzes.t -> Questions.t -> t
    val grade_correct : grade -> bool
    val grade_submission : t -> Submission.t -> ( grade, string ) Result.result
    val grade_submissions : t -> Submissions.t -> ( grade list, string ) Result.result
end = struct
    type t =
        { questions : Questions.t
        ; quizzes : Quizzes.t
        }

    type grade =
        { question : Question.t
        ; correct : bool
        }
    and grades = grade list
    and problem =
        { error : string }
        [@@deriving yojson]

    let result_to_yojson success = function
        | Result.Ok good -> success good
        | Result.Error error -> problem_to_yojson {error}

    let grader_to_yojson = result_to_yojson grade_to_yojson

    let graders_to_yojson = result_to_yojson grades_to_yojson

    let grade_correct g = g.correct

    let create quizzes questions =
        { questions; quizzes }

    let grade_submission t submission =
        let question_id = Submission.question_id submission
        and answer = Submission.answer submission
        in
        try
            let question = Questions.get question_id t.questions in
            let response = 
                { question
                ; correct = Question.answer_is ~answer question
                }
            in Result.Ok response
        with Not_found ->
            Result.Error ( Printf.sprintf "Question %d not found" question_id )

    let grade_submissions t submissions =
        let open Rresult in

        let quiz =
            let quiz_id = submissions.Submissions.quiz_id in
            try
                Result.Ok ( Quizzes.get quiz_id t.quizzes )
            with Not_found ->
                Result.Error ( Printf.sprintf "Quiz %d not found" quiz_id )
        in
        let quiz_questions = quiz >>= fun quiz' ->
            let questions = Questions.questions t.questions in
            try
                Result.Ok ( Quiz.questions ~questions quiz' )
            with Not_found ->
                Result.Error "Questions for quiz not found"
        in

        let quiz_submissions = quiz_questions >>= fun quiz_questions' ->
            let submission_of_question question =
                List.find
                    ( Submission.question_id_is question.Question.id )
                    submissions.Submissions.submissions
            in
            try
                Result.Ok ( List.map submission_of_question quiz_questions' )
            with Not_found ->
                Result.Error "Submission not provided for necessary question"
        in

        quiz_submissions >>= fold_result ( grade_submission t )
end
