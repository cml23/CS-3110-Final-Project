type piece = {
  player : int;
  id : int;
  is_royal : bool;
}
(** The type representing pieces. *)

type t
(** The abstract type of values representing boards. *)

val init_board : t
(** [init_board] is the abstract representation of the default 8x8 board
    at the start of the game. *)

(*val from_json : Yojson.Basic.t -> t*)
(** [from_json j] is the board that [j] represents. Requires: [j] is a
    valid JSON board representation. *)

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