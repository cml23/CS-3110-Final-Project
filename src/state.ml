(* Note: You may introduce new code anywhere in this file. *)

(* TODO: replace [unit] with a type of your own design. *)
type t = unit

let init_state adv = raise (Failure "Unimplemented: State.init_state")

let current_room_id st =
  raise (Failure "Unimplemented: State.current_room_id")

let visited st = raise (Failure "Unimplemented: State.visited")

type result =
  | Legal of t
  | Illegal

let go ex adv st = raise (Failure "Unimplemented: State.go")
