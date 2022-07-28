open Creature

type t = {
  inventory : Creature.t list;
  items : string list;
  curr_cr : Creature.t option;
  win : bool
}

exception NoItems

exception NoCreatures

let init_player = {
  inventory = [];
  items = [];
  curr_cr = None;
  win = false
}

let get_items player = player.items

let owned_creatures player = player.inventory

let get_curr player = player.curr_cr

let get_win player = player.win

let set_items player it = {
  inventory = player.inventory;
  items = it;
  curr_cr = player.curr_cr;
  win = player.win
}

let set_inv player inv = {
  inventory = inv;
  items = player.items;
  curr_cr = player.curr_cr;
  win = player.win
}

let add_item player it = {
  inventory = player.inventory;
  items = it::player.items;
  curr_cr = player.curr_cr;
  win = player.win
}

let add_creature player creature = {
  inventory = creature::player.inventory;
  items = player.items;
  curr_cr = player.curr_cr;
  win = player.win
}

let set_curr player cr = {
  inventory = player.inventory;
  items = player.items;
  curr_cr = cr;
  win = player.win
}

let set_win player w = {
  inventory = player.inventory;
  items = player.items;
  curr_cr = player.curr_cr;
  win = w
}

let rem_item player it lst =
  let rec rem_item_helper p it lst acc =
    match lst with
    |h::t -> if (String.compare h it = 0) then acc @ t
      else rem_item_helper player it t (h::acc)
    |[] -> raise NoItems
  in
  rem_item_helper player it lst []

let rem_cr player cr lst =
  let rec rem_cr_helper p cr lst acc =
    match lst with
    |h::t -> if (String.compare (creature_nickname h) cr = 0) then acc@t
      else rem_cr_helper p cr t (h::acc)
    |[] -> raise NoCreatures
  in
  rem_cr_helper player cr lst []

(** [list_items list] is a function that determines whether or not to insert a 
    commma or "and" in a list of strings [list] *)
let rec list_items list =
  match list with
  | [] -> ""
  | l::[] -> "and " ^ l
  | h::t -> h ^ ", " ^ list_items t

let items player =
  match player.items with
  | [] -> ""
  | l::[] -> String.concat "" player.items
  | h::m::[] -> String.concat " and " player.items
  | h::t -> list_items player.items

(**[list_cr list] is a function that formats the list of creatures [list] to be
    printed. *)
let rec list_cr list =
  match list with
  |[] -> ""
  |l::[] -> "and (" ^ (creature_nickname l) ^ ", " ^ (creature_name l) ^ 
            ", Lvl. " ^ string_of_int (creature_level l) ^ ")"
  |h::t -> "(" ^ (creature_nickname h) ^ ", " ^ (creature_name h) ^ ", Lvl. " ^ 
           string_of_int (creature_level h) ^ "), " ^ list_cr t

let creatures player =
  match player.inventory with
  |[] -> ""
  |l::[] -> "(" ^ (creature_nickname l) ^ ", " ^ (creature_name l) ^ ", Lvl. " ^ 
            string_of_int (creature_level l) ^ ")"
  |h::m::[] -> "(" ^ (creature_nickname h) ^ ", " ^ (creature_name h) ^ 
               ", Lvl. " ^ string_of_int (creature_level h) ^ ") and (" ^ 
               (creature_nickname m) ^ ", " ^ (creature_name m) ^ ", Lvl. " ^ 
               string_of_int (creature_level m) ^ ")"     
  |h::t -> list_cr player.inventory

let won p = if List.length (owned_creatures p) >= 7 then true else false