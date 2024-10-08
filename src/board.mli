(** Representation of the board and its pieces. *)

type piece = {
  player : int;
  id : int;
  is_royal : bool;
}
(** The type representing pieces. *)

type t
(** The abstract type of values representing boards. *)

val get_x : t -> int -> int
(** [get_x b i] is the x coordinate on board [b] corresponding to the
    general index [i], which is 1 at the bottom left and maximum at the
    top right. For example, on a standard 8x8 board, the x coordinate
    for index 24 is 8. *)

val get_y : t -> int -> int
(** [get_y b i] is the y coordinate on board [b] corresponding to the
    general index [i], which is 1 at the bottom left and maximum at the
    top right. For example, on a standard 8x8 board, the y coordinate
    for index 24 is 3. *)

val get_idx : t -> int -> int -> int
(** [get_idx b x y] is the general index corresponding to coordinates x
    and y on board b. *)

val dim_x : t -> int
(** [dim_x b] is the x-dimension of board b. *)

val dim_y : t -> int
(** [dim_y b] is the y-dimension of board b. *)

val piece_of_xy : t -> int -> int -> piece option
(** [piece_of_idx b x y] is Some the piece located at position (x,y) on
    board b if a piece exists at that position; otherwise it is None. *)

val pieces_of_player : t -> int -> piece list
(** [pieces_of_player b pl] is the list of pieces on board [b] belonging
    to player [pl]. *)

val num_pcs_of_pl : t -> int -> int
(** [num_pcs_of_pl b pl] is the number of pieces on board [b] belonging
    to player [pl]. *)

val xy_of_pc : t -> piece -> (int * int) option
(** [xy_of_pc b pc] is Some (x,y) where x and y are the coordinates of
    piece [pc] on board [b] if it exists; otherwise the result is None. *)

val up_r : t -> int -> int -> (int * int) option
(** [up_r b x y] is Some (x_new, y_new) where x_new and y_new are the
    coordinates of the tile to the upper right of the tile at (x, y) if
    the new tile exists; otherwise the result is None. *)

val up_l : t -> int -> int -> (int * int) option
(** [up_l b x y] is Some (x_new, y_new) where x_new and y_new are the
    coordinates of the tile to the upper left of the tile at (x, y) if
    the new tile exists; otherwise the result is None. *)

val down_r : t -> int -> int -> (int * int) option
(** [down_r b x y] is Some (x_new, y_new) where x_new and y_new are the
    coordinates of the tile to the lower right of the tile at (x, y) if
    the new tile exists; otherwise the result is None. *)

val down_l : t -> int -> int -> (int * int) option
(** [down_l b x y] is Some (x_new, y_new) where x_new and y_new are the
    coordinates of the tile to the lower left of the tile at (x, y) if
    the new tile exists; otherwise the result is None. *)

val copy_bd : t -> t
(** [copy_bd b] is a copy of board [b]. *)

val del_pc : t -> int -> int -> t
(** [del_pc b x y] is board [b] with the tile at position x,y set to
    None. *)

val pc_exists : t -> int -> int -> bool
(** [pc_exists b x y] is true iff there is a piece at position x,y in
    board [b]. *)

val poss_moves : t -> piece -> (int * int) list
(** [poss_moves b pc] is the list of moves that piece [pc] can make on
    board [b], excluding piece-capturing moves. *)

val poss_captures : t -> piece -> (int * int) list * piece list
(** [poss_captures b pc] is a pair where the first value is the list of
    possible x,y locations piece [pc] could move to on board [b] by way
    of legal single jump captures; the second value is the list of
    pieces that would be captured by the corresponding move. *)

val add_pc : t -> piece -> int -> int -> t
(** [add_pc b pc x y] is board [b] with piece [pc] added to position x,y
    if that piece is empty; otherwise raises NotEmpty. *)

val promote_pc : t -> piece -> t
(** [promote b pc] is [b] with the [pc.is_royal] set to true for piece
    [pc] on board [b]. *)

val is_promotable : t -> piece -> bool
(** [is_promotable b pc] is whether piece [pc] on board [b] is
    promotable. This is true if [pc.is_royal=false] and [pc] is at the
    opposite end of the board from which it started. *)

val to_json : t -> Yojson.Basic.t
(** [to_json b] is the json representation of board [b]. *)

val from_json : Yojson.Basic.t -> t
(**[from_json json] is the board represented by [json]. Requires: [json]
   is a valid JSON representation of a board.*)
