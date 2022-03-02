(** Representation of static adventure data.

    This module represents the data stored in adventure files, including
    the rooms and exits. It handles loading of that data from JSON as
    well as querying the data. *)

(**********************************************************************
 * DO NOT CHANGE THIS FILE
 * It is part of the interface the course staff will use to test your
 * submission.
 **********************************************************************)

type t
(** The abstract type of values representing adventures. *)

exception UnknownRoom of string
(** Raised when an unknown room is encountered. *)

exception UnknownExit of string
(** Raised when an unknown exit is encountered. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the adventure that [j] represents. Requires: [j] is
    a valid JSON adventure representation. *)

val start_room : t -> string
(** [start_room a] is the identifier of the starting room in adventure
    [a]. *)

val room_ids : t -> string list
(** [room_ids a] is a set-like list of all of the room identifiers in
    adventure [a]. *)

val description : t -> string -> string
(** [description a r] is the description of room [r] in adventure [a].
    Raises [UnknownRoom r] if [r] is not a room identifier in [a]. *)

val exits : t -> string -> string list
(** [exits a r] is a set-like list of all exit names from room [r] in
    adventure [a]. Raises [UnknownRoom r] if [r] is not a room
    identifier in [a]. *)

val next_room : t -> string -> string -> string
(** [next_room a r e] is the room to which [e] exits from room [r] in
    adventure [a]. Raises [UnknownRoom r] if [r] is not a room
    identifier in [a]. Raises [UnknownExit e] if [e] is not an exit from
    room [r] in [a]. *)

val next_rooms : t -> string -> string list
(** [next_rooms a r] is a set-like list of all rooms to which there is
    an exit from [r] in adventure [a]. Raises [UnknownRoom r] if [r] is
    not a room identifier in [a].*)
