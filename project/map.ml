open Yojson.Basic.Util

type tile = Path | Tree | Grass
type coord = int * int

type t = {
  map : char list list; 
  init_pos : coord; 
  height: int; 
  width : int;
  creatures : Creature.t list;
  items : string list
}

exception InvalidTile

exception NoCreature

exception NoItem

let from_json j =
  let char_of_string str = String.get str 0 in
  let assoc = to_assoc j in
  {
    map = assoc |> List.assoc "map" |> to_list |>
          List.map to_assoc |> List.map (List.map snd) |> 
          List.map (List.map to_string) |>
          List.map (List.map char_of_string);

    init_pos = (assoc |> List.assoc "start position x" |> to_int,
                assoc |> List.assoc "start position y" |> to_int);

    height = assoc |> List.assoc "map height" |> to_int;

    width = assoc |> List.assoc "map width" |> to_int;

    items = assoc |> List.assoc "items" |> to_list |> List.map to_string;

    creatures = let clist = assoc |> List.assoc "creatures" |> to_list in
      List.map Creature.c_from_json clist;
  }

let map_items map = map.items

let all_creatures map = map.creatures

(** [pick_creature' creatures name] is a helper function that returns the 
    creature with name [name] from the list of creatures [creatures].
    Raises: [NoCreature] exception if there are no creatures in the list. *)
let rec pick_creature' creatures name =
  match creatures with
  | h::t -> if Creature.creature_name h = name
    then h
    else pick_creature' t name
  | [] -> raise NoCreature

let pick_creature map name = pick_creature' (all_creatures map) name

let legal_position map pos = fst pos > -1 && fst pos < map.width &&
                             snd pos > -1 && snd pos < map.height

let initial_position map = map.init_pos

(** [tile_of_char c] is a function that returns the tile associated with 
    character [c]. *)
let tile_of_char = function
  | '.' -> Path
  | '!' -> Tree
  | '#' -> Grass
  | _ -> raise InvalidTile

let char_at_position map pos =
  let x = fst pos in
  let y = snd pos in
  List.nth (List.nth map.map y) x

let tile_at_position map pos = char_at_position map pos |> tile_of_char

(** [get_row map x y acc] is a function that returns the list of characters in 
    the row at height [y] starting at width [x] in map [map]. *)
let rec get_row map x y acc =
  if List.length acc >= 5 then acc else
  if legal_position map (x,y) then get_row map (x-1) y
      ((char_at_position map (x,y))::acc) else
    get_row map (x-1) y
      acc

let viewable_from_position map pos =
  let x = fst pos in
  let y = snd pos in
  let clamp_pos = ((min (max x 2) 12), (min (max y 2) 12)) in
  let cx = fst clamp_pos in
  let cy = snd clamp_pos in
  let row0 = get_row map (cx + 2) (cy - 2) [] in
  let row1 = get_row map (cx + 2) (cy - 1) [] in
  let row2 = get_row map (cx + 2) (cy - 0) [] in
  let row3 = get_row map (cx + 2) (cy + 1) [] in
  let row4 = get_row map (cx + 2) (cy + 2) [] in

  [row0; row1; row2; row3; row4]

let encounter m pos cr_list =
  let r = Random.int(2) in
  let x = Random.int (List.length cr_list) in
  if tile_at_position m pos = Grass && r = 1
  then List.nth_opt cr_list x
  else None

let get_cr cr = 
  match cr with
  |Some v -> v
  |None -> raise NoCreature

let item_enc m pos it_list =
  let r = Random.int(10) in
  let x = Random.int (List.length it_list) in
  if tile_at_position m pos = Path && r < 4
  then List.nth_opt it_list x
  else None

let get_it it = 
  match it with
  |Some i -> i
  |None -> raise NoItem
