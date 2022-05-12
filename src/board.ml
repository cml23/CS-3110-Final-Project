exception NotEmpty

type piece = {
  player : int;
  id : int;
  is_royal : bool;
}
(** Pieces are represented with a player number (1 or 2), a unique id,
    and whether or not they are royal. *)

type tile = piece option
(** A tile is represented as a piece option where empty tiles are
    represented as [None] and nonempty tiles are represented as
    [Some pc] where [pc] is the piece located on that tile. *)

type t = tile array * int
(** A board can have any rectangular shape. The number of tiles per row
    (i.e., the number of columns) is given by the second number in the
    tuple representation. *)

let get_x (b : t) (i : int) : int = ((i - 1) mod snd b) + 1
let get_y (b : t) (i : int) : int = ((i - 1) / snd b) + 1
let get_idx (b : t) (x : int) (y : int) : int = (snd b * (y - 1)) + x
let dim_x (b : t) : int = snd b
let dim_y (b : t) : int = Array.length (fst b) / snd b

let piece_of_xy (b : t) x y =
  match Array.get (fst b) (get_idx b x y - 1) with
  | None -> (None : piece option)
  | Some pc -> (Some pc : piece option)

let pieces_of_player (b : t) pl =
  Array.fold_left
    (fun acc el ->
      match el with
      | None -> acc
      | Some p -> if p.player = pl then p :: acc else acc)
    [] (fst b)

let num_pcs_of_pl (b : t) (pl : int) : int =
  List.length (pieces_of_player b pl)

(** [idx_pc_aux tiles pc i] is None if tile [Some pc] is not present in
    [tiles] and [Some i] if tile [Some pc] is present in [tiles] at
    index [i]. *)
let rec idx_pc_aux (tiles : tile array) (pc : piece) (i : int) :
    int option =
  if i >= Array.length tiles then None
  else if tiles.(i) = Some pc then Some (i + 1)
  else idx_pc_aux tiles pc (i + 1)

let xy_of_pc (b : t) pc =
  let idx = idx_pc_aux (fst b) pc 0 in
  match idx with
  | None -> None
  | Some i -> Some (get_x b i, get_y b i)

let up_r (b : t) (x : int) (y : int) : (int * int) option =
  if x < dim_x b && y < dim_y b then Some (x + 1, y + 1) else None

let up_l (b : t) (x : int) (y : int) : (int * int) option =
  if x > 1 && y < dim_y b then Some (x - 1, y + 1) else None

let down_r (b : t) (x : int) (y : int) : (int * int) option =
  if x < dim_x b && y > 1 then Some (x + 1, y - 1) else None

let down_l (b : t) (x : int) (y : int) : (int * int) option =
  if x > 1 && y > 1 then Some (x - 1, y - 1) else None

let copy_bd (b : t) : t = (Array.copy (fst b), snd b)

let del_pc b x y =
  let new_bd = copy_bd b in
  Array.set (fst new_bd) (get_idx new_bd x y - 1) None;
  new_bd

let pc_exists b x y =
  match piece_of_xy b x y with
  | None -> false
  | Some _ -> true

(** [get_neighbors b pc x y f_n] is the list of neighbors (locations
    that piece [pc] at location x,y could move to on board [b]) in the
    direction given by [f_n]. *)
let get_neighbor
    (b : t)
    (pc : piece)
    (x : int)
    (y : int)
    (f_n : t -> int -> int -> (int * int) option) : (int * int) list =
  match f_n b x y with
  | None -> []
  | Some (x_new, y_new) -> (
      match piece_of_xy b x_new y_new with
      | None -> [ (x_new, y_new) ]
      | Some _ -> [])

let poss_moves (b : t) (pc : piece) : (int * int) list =
  match xy_of_pc b pc with
  | None -> []
  | Some (x, y) ->
      let up_moves =
        get_neighbor b pc x y up_r @ get_neighbor b pc x y up_l
      in
      let down_moves =
        get_neighbor b pc x y down_r @ get_neighbor b pc x y down_l
      in
      if pc.is_royal then up_moves @ down_moves
      else if pc.player = 1 then up_moves
      else down_moves

(** [capture_help b pc f_n x1 y1] produces the output for [capture] in
    the case that the piece does have a neighboring tile in the desired
    direction, located at (x1,y1). *)
let capture_help
    (b : t)
    (pc : piece)
    (f_n : t -> int -> int -> (int * int) option)
    (x1 : int)
    (y1 : int) : (int * int) list * piece list =
  match piece_of_xy b x1 y1 with
  | None -> ([], [])
  | Some pc_opp when pc_opp.player <> pc.player -> (
      match f_n b x1 y1 with
      | None -> ([], [])
      | Some (x2, y2) when piece_of_xy b x2 y2 = None ->
          ([ (x2, y2) ], [ pc_opp ])
      | Some _ -> ([], []))
  | Some _ -> ([], [])

(** [capture b pc f_n] is a pair of empty lists if there is no valid
    capture for piece [pc] on board [b] in the direction specified by
    [f_n]; otherwise it is a pair of singleton lists where the first
    list contains the new x,y position that [pc] would move to by making
    a capture in the direction of [f_n] and the second list is the
    captured piece. *)
let capture
    (b : t)
    (pc : piece)
    (f_n : t -> int -> int -> (int * int) option) :
    (int * int) list * piece list =
  match xy_of_pc b pc with
  | None -> ([], [])
  | Some (x, y) -> (
      match f_n b x y with
      | None -> ([], [])
      | Some (x1, y1) -> capture_help b pc f_n x1 y1)

let poss_captures (b : t) (pc : piece) : (int * int) list * piece list =
  let up_r_captures = capture b pc up_r in
  let up_l_captures = capture b pc up_l in
  let up_captures =
    ( fst up_r_captures @ fst up_l_captures,
      snd up_r_captures @ snd up_l_captures )
  in
  let down_r_captures = capture b pc down_r in
  let down_l_captures = capture b pc down_l in
  let down_captures =
    ( fst down_r_captures @ fst down_l_captures,
      snd down_r_captures @ snd down_l_captures )
  in
  if pc.is_royal then
    ( fst up_captures @ fst down_captures,
      snd up_captures @ snd down_captures )
  else if pc.player = 1 then up_captures
  else down_captures

let add_pc (b : t) (pc : piece) (x : int) (y : int) : t =
  match piece_of_xy b x y with
  | None ->
      let new_bd = copy_bd b in
      Array.set (fst new_bd) (get_idx new_bd x y - 1) (Some pc);
      new_bd
  | Some _ -> raise NotEmpty

let promote_pc (b : t) (pc : piece) =
  let new_bd = copy_bd b in
  let x, y =
    match xy_of_pc b pc with
    | None -> failwith "Nonexistent piece."
    | Some (x_pc, y_pc) -> (x_pc, y_pc)
  in
  Array.set (fst new_bd)
    (get_idx new_bd x y - 1)
    (Some { pc with is_royal = true });
  new_bd

let is_promotable (b : t) (pc : piece) =
  (not pc.is_royal)
  &&
  match xy_of_pc b pc with
  | None -> failwith "Nonexistent piece."
  | Some (_, y) ->
      let row_max = Array.length (fst b) / snd b in
      (pc.player = 1 && y = row_max) || (pc.player = 2 && y = 1)

(** [json_of_tiles tiles] is the json represented by [tiles]. *)
let json_of_tiles (tiles : tile array) : Yojson.Basic.t list =
  let lst = ref [] in
  for i = 0 to Array.length tiles - 1 do
    match tiles.(i) with
    | None -> ()
    | Some { player; id; is_royal } ->
        lst :=
          !lst
          @ [
              `Assoc
                [
                  ("player", `Int player);
                  ("id", `Int id);
                  ("tile", `Int (i + 1));
                  ("is_royal", `Bool is_royal);
                ];
            ]
  done;
  !lst

let to_json (b : t) : Yojson.Basic.t =
  `Assoc
    [
      ("tiles", `List (json_of_tiles (fst b)));
      ("columns", `Int (snd b));
      ("rows", `Int (Array.length (fst b) / snd b));
    ]

(* FUNCTIONS ADDED BY CASSIDY BELOW. *)

(**[tile_of_json json] is the tile represented by [json]. Requires:
   [json] is a valid tile representation.*)
let tile_of_json (a : tile array) json : unit =
  let open Yojson.Basic.Util in
  let pos = json |> member "tile" |> to_int in
  let tile =
    Some
      {
        player = json |> member "player" |> to_int;
        id = json |> member "id" |> to_int;
        is_royal = json |> member "is_royal" |> to_bool;
      }
  in
  a.(pos - 1) <- tile

let from_json (json : Yojson.Basic.t) : t =
  let open Yojson.Basic.Util in
  let rows = json |> member "rows" |> to_int in
  let cols = json |> member "columns" |> to_int in
  let a = Array.make (rows * cols) None in
  let _ =
    json |> member "tiles" |> to_list |> List.iter (tile_of_json a)
  in
  (a, cols)
(* END FUNCTIONS ADDED BY CASSIDY. *)