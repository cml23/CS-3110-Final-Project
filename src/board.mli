type t
(** The abstract type of values representing boards. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the board that [j] represents. Requires: [j] is a
    valid JSON board representation. *)

val pieces_of_player : t -> string -> string list
(** [pieces_of_player b pl] is the list of pieces on board [b] belonging
    to player [pl]*)
