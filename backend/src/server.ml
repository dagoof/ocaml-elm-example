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

(* Middleware to add CORS to all requests, really need this for OPTIONS
 * preflight checks
 *)
let add_cors_headers =
    let filter handler req =
        handler req >|= fun response ->
            { response with Response.headers =
                Cohttp.Header.add_list response.Response.headers @@
                cors_headers req.Request.request
            }
    in
    Rock.Middleware.create ~name:"cors headers" ~filter

(* We could do this (and I originally did) in an Lwt thread and keep them in
 * the app middleware, but it really simplifies things to just load our data
 * at startup and bail out early if anything goes wrong.
 *)
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

(* Encode out a set of json, output can be controlled by providing an alternate
 * returner.
 *
 * Use *respond* to get back a Opium.Rock.Response outside of the Lwt monad
 *)
let render_json ?(returner=respond') encoder data =
    encoder data
    |> Yojson.Safe.to_string
    |> (fun s -> returner ~headers:json_headers @@ `String s )

(* All the questions, straight from json *)
let questions = get "/questions" begin fun req ->
    render_json Model.Questions.to_yojson question_data
end

(* All the quizzes, straight from json *)
let quizzes = get "/quizzes" begin fun req ->
    render_json Model.Quizzes.to_yojson quiz_data
end

(* Grade a single question
 *
 * reads like this:
     * create a grader
     * parse provided json submission and use that to
     * grade the submission against our grader
     * render out a single grading response
 *)
let grade = post "/grade" begin fun req ->
    let grader = Model.Grading.create quiz_data question_data in

    parse_json Model.Submission.of_yojson req >>= begin fun submission ->
        render_json Model.Grading.grader_to_yojson @@
        Model.Grading.grade_submission grader submission
    end
end

(* Grade an entire quiz answer *)
let grade_quiz = post "/grade_quiz" begin fun req ->
    let grader = Model.Grading.create quiz_data question_data in

    parse_json Model.Submissions.of_yojson req >>= begin fun submissions ->
        render_json Model.Grading.graders_to_yojson @@
        Model.Grading.grade_submissions grader submissions
    end
end

(* Apply all of our routes, middleware, and run the app *)
let () =
    App.empty
    |> questions
    |> quizzes
    |> grade
    |> grade_quiz
    |> middleware add_cors_headers
    |> App.run_command
