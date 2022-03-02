type object_phrase = string list

type command =
  | Go of object_phrase
  | Quit

exception Empty
exception Malformed

let parse str = raise (Failure "Command.parse")
