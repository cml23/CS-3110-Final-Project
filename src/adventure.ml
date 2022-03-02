(* Note: You may introduce new code anywhere in this file. *)

exception UnknownRoom of string
exception UnknownExit of string

(* TODO: replace [unit] with a type of your own design. *)
type t = unit

let from_json json =
  raise (Failure "Unimplemented: Adventure.from_json")

let start_room adv =
  raise (Failure "Unimplemented: Adventure.start_room")

let room_ids adv = raise (Failure "Unimplemented: Adventure.room_ids")

let description adv room =
  raise (Failure "Unimplemented: Adventure.description")

let exits adv room = raise (Failure "Unimplemented: Adventure.exits")

let next_room adv room ex =
  raise (Failure "Unimplemented: Adventure.next_room")

let next_rooms adv room =
  raise (Failure "Unimplemented: Adventure.next_rooms")
