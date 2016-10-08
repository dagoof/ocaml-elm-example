open Lwt
open Opium.Std

let (<<) f g x = f @@ g x

let cors_headers req =
    [ "Access-Control-Allow-Origin", "http://localhost:8085"
    ; "Access-Control-Allow-Credentials", "true"
    ; "Access-Control-Allow-Methods", Cohttp.Code.string_of_method @@ Cohttp.Request.meth req
    ]

let json_headers =
    Cohttp.Header.add_list
    ( Cohttp.Header.init () )
    [ "Content-Type", "application/json; charset=utf-8" ]

let add_cors_headers =
    let filter handler req =
        handler req >|= fun response ->
            { response with Response.headers =
                Cohttp.Header.add_list response.Response.headers @@
                cors_headers req.Request.request
            }
    in
    Rock.Middleware.create ~name:"cors headers" ~filter

let load_json_file decoder fname =
    let open Batteries in
    File.with_file_in fname IO.read_all
    |> decoder
    |> Rresult.R.get_ok

let parse_json decoder req =
    App.string_of_body_exn req >|= begin fun body ->
        body
        |> Yojson.Safe.from_string
        |> decoder
        |> Rresult.R.get_ok
    end

let question_data = load_json_file
    ( Model.Questions.of_yojson << Yojson.Safe.from_string )
    "resources/questions.json"

and quiz_data = load_json_file
    ( Model.Quizzes.of_yojson << Yojson.Safe.from_string )
    "resources/quizzes.json"

let render_json ?(returner=respond') encoder data =
    encoder data
    |> Yojson.Safe.to_string
    |> (fun s -> returner ~headers:json_headers @@ `String s )

let questions = get "/questions" begin fun req ->
    render_json Model.Questions.to_yojson question_data
end

let quizzes = get "/quizzes" begin fun req ->
    render_json Model.Quizzes.to_yojson quiz_data
end

let grade = post "/grade" begin fun req ->
    let grader = Model.Grading.create quiz_data question_data in

    parse_json Model.Submission.of_yojson req >>= begin fun submission ->
        render_json Model.Grading.grader_to_yojson @@
        Model.Grading.grade_submission grader submission
    end
end

let grade_quiz = post "/grade_quiz" begin fun req ->
    let grader = Model.Grading.create quiz_data question_data in

    parse_json Model.Submissions.of_yojson req >>= begin fun submissions ->
        render_json Model.Grading.graders_to_yojson @@
        Model.Grading.grade_submissions grader submissions
    end
end

let () =
    App.empty
    |> questions
    |> quizzes
    |> grade
    |> grade_quiz
    |> middleware add_cors_headers
    |> App.run_command
