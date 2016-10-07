open Lwt
open Opium.Std

let (<<) f g x = f @@ g x

let json_headers =
    Cohttp.Header.init_with
    "Content-Type"
    "application/json; charset=utf-8"

let load_json decoder fname =
    let open Batteries in
    File.with_file_in fname IO.read_all
    |> decoder
    |> Rresult.R.get_ok

let question_data = load_json
    ( Model.Questions.of_yojson << Yojson.Safe.from_string )
    "resources/questions.json"

and quiz_data = load_json
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

let () =
    App.empty
    |> questions
    |> quizzes
    |> App.run_command
