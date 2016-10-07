open Opium.Std

let questions = get "/questions" begin fun req ->
    respond' @@ `String "questions"
end

let quizzes = get "/quizzes" begin fun req ->
    respond' @@ `String "quizzes"
end

let () =
    App.empty
    |> questions
    |> quizzes
    |> App.run_command
