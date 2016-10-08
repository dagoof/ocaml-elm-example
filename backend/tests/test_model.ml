open Model

let suite =
    let question_a =
        { Question.id = 0
        ; question = "Question A"
        ; answers = [ "a"; "b"; "c" ]
        ; correct_answer = 0
        }
    and question_b =
        { Question.id = 1
        ; question = "Question B"
        ; answers = [ "a"; "b"; "c" ]
        ; correct_answer = 2
        }
    and question_c =
        { Question.id = 2
        ; question = "Question C"
        ; answers = [ "a"; "b"; "c"; "d" ]
        ; correct_answer = 1
        }
    and question_d =
        { Question.id = 3
        ; question = "Question D"
        ; answers = [ "a"; "b"; "c"; "d" ]
        ; correct_answer = 3
        }
    and quiz_a =
        { Quiz.id = 1
        ; title = "Quiz A"
        ; question_ids = [ 0;1;2 ]
        }
    and quiz_b =
        { Quiz.id = 2
        ; title = "Quiz B"
        ; question_ids = [ 2;3 ]
        }
    in
    let questions =
        { Questions.questions =
            [ question_a
            ; question_b
            ; question_c
            ; question_d
            ]
        }
    and quizzes =
        { Quizzes.quizzes =
            [ quiz_a
            ; quiz_b
            ]
        }
    in questions, quizzes

let always_unit x = ()

let grade_success grader submission expected () =
    let grade = Grading.grade_submission grader submission in
    let grade = Rresult.R.map Grading.grade_correct grade in
    Alcotest.(check @@ result bool string) "grader output" expected grade

let grades_success grader submissions expected () =
    let grade = Grading.grade_submissions grader submissions in
    let grade = Rresult.R.map ( List.map Grading.grade_correct ) grade in
    Alcotest.(check @@ result (list bool) string) "grader output" expected grade

let model_helpers =
    let ( questions, quizzes ) = suite in
    let question_id = 1 in
    let question = Questions.get question_id questions in
    [ "Question lookup gets correct question", `Quick, begin fun () ->
        Alcotest.(check int) "question id" question.Question.id question_id
    end

    ; "Question lookup raises when not found", `Quick, begin fun () ->
        Alcotest.check_raises
            "unknown question id"
            Not_found
            ( fun () -> always_unit @@ Questions.get 213 questions )
    end

    ; "Question lookup raises when not found", `Quick, begin fun () ->
        Alcotest.check_raises
            "unknown question id"
            Not_found
            ( fun () -> always_unit @@ Questions.get 213 questions )
    end
    ]

let grading =
    let ( questions, quizzes ) = suite in
    let grader = Grading.create quizzes questions in
    [ "Good question, good answer", `Quick,
        grade_success grader
            { Submission.question_id = 0
            ; answer = 0
            }
            ( Result.Ok true )

    ; "Good question, bad answer", `Quick,
        grade_success grader
            { Submission.question_id = 0
            ; answer = 1
            }
            ( Result.Ok false )

    ; "Bad question", `Quick,
        grade_success grader
            { Submission.question_id = 213
            ; answer = 0
            }
            ( Result.Error "Question 213 not found" )

    ; "Good quiz, good questions, good answers", `Quick,
        grades_success grader
            { Submissions.quiz_id = 1
            ; submissions =
                [ { Submission.question_id = 0
                  ; answer = 0
                  }
                ; { Submission.question_id = 1
                  ; answer = 2
                  }
                ; { Submission.question_id = 2
                  ; answer = 1
                  }
                ]
            }
            ( Result.Ok [true;true;true] )

    ; "Good quiz, good questions, bad answer", `Quick,
        grades_success grader
            { Submissions.quiz_id = 1
            ; submissions =
                [ { Submission.question_id = 0
                  ; answer = 1
                  }
                ; { Submission.question_id = 1
                  ; answer = 2
                  }
                ; { Submission.question_id = 2
                  ; answer = 1
                  }
                ]
            }
            ( Result.Ok [false;true;true] )

    ; "Good quiz, wrong questions", `Quick,
        grades_success grader
            { Submissions.quiz_id = 1
            ; submissions =
                [ { Submission.question_id = 3
                  ; answer = 3
                  }
                ; { Submission.question_id = 1
                  ; answer = 2
                  }
                ; { Submission.question_id = 2
                  ; answer = 1
                  }
                ]
            }
            ( Result.Error "Submission not provided for necessary question" )


    ; "Bad quiz", `Quick,
        grades_success grader
            { Submissions.quiz_id = 3
            ; submissions =
                [ { Submission.question_id = 213
                  ; answer = 1
                  }
                ; { Submission.question_id = 1
                  ; answer = 2
                  }
                ; { Submission.question_id = 2
                  ; answer = 1
                  }
                ]
            }
            ( Result.Error "Quiz 3 not found" )
    ]

let () =
    Alcotest.run "test_model"
    [ "submissions", grading
    ; "model-module helpers", model_helpers
    ]
