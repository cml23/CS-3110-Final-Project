exception NotEmpty

type piece = {
  player : int;
  id : int;
  is_royal : bool;
}

type tile = piece option
type t = tile array * int
(* a board can have any rectangular shape. The number of tiles per row
   (i.e., the number of columns) is given by the second number in the
   tuple representation. *)

let std_pc p i = Some { player = p; id = i; is_royal = false }

let init_board : t =
  ( [|
      std_pc 1 1;
      None;
      std_pc 1 2;
      None;
      std_pc 1 3;
      None;
      std_pc 1 4;
      None;
      None;
      std_pc 1 5;
      None;
      std_pc 1 6;
      None;
      std_pc 1 7;
      None;
      std_pc 1 8;
      std_pc 1 9;
      None;
      std_pc 1 10;
      None;
      std_pc 1 11;
      None;
      std_pc 1 12;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      None;
      std_pc 2 13;
      None;
      std_pc 2 14;
      None;
      std_pc 2 15;
      None;
      std_pc 2 16;
      std_pc 2 17;
      None;
      std_pc 2 18;
      None;
      std_pc 2 19;
      None;
      std_pc 2 20;
      None;
      None;
      std_pc 2 21;
      None;
      std_pc 2 22;
      None;
      std_pc 2 23;
      None;
      std_pc 2 24;
    |],
    8 )

(*let from_json json = raise (Failure "Unimplemented:
  Board.from_json") *)

let get_x (b : t) i = ((i - 1) mod snd b) + 1
let get_y (b : t) i = ((i - 1) / snd b) + 1
let get_idx (b : t) x y = (snd b * (y - 1)) + x
let dim_x (b : t) = snd b
let dim_y (b : t) = Array.length (fst b) / snd b

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

let num_pcs_of_pl b pl = List.length (pieces_of_player b pl)

let rec idx_pc_aux tiles pc i : int option =
  if i >= Array.length tiles then None
  else if tiles.(i) = Some pc then Some (i + 1)
  else idx_pc_aux tiles pc (i + 1)

let xy_of_pc (b : t) pc =
  let idx = idx_pc_aux (fst b) pc 0 in
  match idx with
  | None -> None
  | Some i -> Some (get_x b i, get_y b i)

let up_r b x y =
  if x < dim_x b && y < dim_y b then Some (x + 1, y + 1) else None

let up_l b x y =
  if x > 1 && y < dim_y b then Some (x - 1, y + 1) else None

let down_r b x y =
  if x < dim_x b && y > 1 then Some (x + 1, y - 1) else None

let down_l (b : t) x y =
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
(* [get_neighbors b pc x y f_n] is the list of neighbors (locations that
   piece [pc] at location x,y could move to on board [b]) in the
   direction given by [f_n]. *)

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
      | Some (x1, y1) -> (
          match piece_of_xy b x1 y1 with
          | None -> ([], [])
          | Some pc_opp when pc_opp.player <> pc.player -> (
              match f_n b x1 y1 with
              | None -> ([], [])
              | Some (x2, y2) when piece_of_xy b x2 y2 = None ->
                  ([ (x2, y2) ], [ pc_opp ])
              | Some _ -> ([], []))
          | Some _ -> ([], [])))
(* [capture b pc f_n] is a pair of empty lists if there is no valid
   capture for piece [pc] on board [b] in the direction specified by
   [f_n]; otherwise it is a pair of singleton lists where the first list
   contains the new x,y position that [pc] would move to by making a
   capture in the direction of [f_n] and the second list is the captured
   piece. *)

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

(* TODO: Encapsulate 4 neighbor functions, get_x, get_y, get_idx. *)

(* TODO: add tests for poss_move, poss_captures, add_pc. *)

(* FUNCTIONS ADDED BY CASSIDY BELOW. *)

(* END FUNCTIONS ADDED BY CASSIDY. *)