module Quiz = struct
    type t = 
        { id : int
        ; title : string
        ; question_ids : int list
        } [@@deriving yojson]
end

module Question = struct
    type t =
        { id : int
        ; question : string
        ; answers : string list
        ; correct_answer : int
        } [@@deriving yojson]
end

module Questions = struct
    type t =
        { questions : Question.t list }
        [@@deriving yojson]
end

module Quizzes = struct
    type t =
        { quizzes : Quiz.t list }
        [@@deriving yojson]
end
