open Yojson.Basic.Util

type ptype = Fire | Grass | Water

type attack = {
  mtype : string;
  name : string;
  power : int;
}

type t = {
  name : string;
  my_type : string;
  attacks: attack list;
  level : int;
  experience : int;
  exp_for_lvl_up : int;
  health_points : int;
  nickname : string
}

let attack_from_json json =
  {
    mtype = json |> member "mtype" |> to_string;
    name = json |> member "name" |> to_string;
    power = json |>member "power" |> to_int;
  }

let c_from_json json =
  {
    name = json |> member "name" |> to_string;
    my_type = json |> member "my_type" |> to_string;
    attacks = json |> member "attacks" |> to_list |> List.map attack_from_json;
    level = json |> member "level" |> to_int;
    experience = json |> member "experience" |> to_int;
    exp_for_lvl_up = json |> member "exp_for_lvl_up" |> to_int;
    health_points = json |> member "health_points" |> to_int;
    nickname = json |> member "nickname" |> to_string;
  }

let str_to_ptype str=
  match str with
  |"fire" -> Fire
  |"water" -> Water
  |"grass" -> Grass
  |_ -> failwith "Not a valid type"

let creature_name cr = cr.name

let cr_nickname cr = cr.nickname

(**[creature_type_str c] is the string that can be translated to a ptype*)
let creature_type_str cr = cr.my_type

let creature_type cr = 
  cr |> creature_type_str |> str_to_ptype

let creature_attacks cr = cr.attacks

let creature_level cr = cr.level

let creature_exp cr = cr.experience

let creature_exp_lvl_up cr = cr.exp_for_lvl_up

let creature_max_hp cr = cr.health_points

let creature_nickname cr = cr.nickname

let set_nickname cr n = {
  name = cr.name;
  my_type = cr.my_type;
  attacks = cr.attacks;
  level = cr.level;
  experience = cr.experience;
  exp_for_lvl_up = cr.exp_for_lvl_up;
  health_points = cr.health_points;
  nickname = n
}

let attack_name (att:attack) = att.name

let attack_power att = att.power

(**[attack_type_str att] is the string that can be translated to a ptype*)
let attack_type_str att = att.mtype

let attack_type att =
  att.mtype |> str_to_ptype

let level_up cr exp lvl =
  let curr_exp = exp/50 in
  if lvl <= curr_exp
  then lvl + 1
  else lvl

let rem_exp cr exp =
  if exp >= cr.exp_for_lvl_up
  then exp - cr.exp_for_lvl_up
  else cr.exp_for_lvl_up - exp

(** [lvl_up_hp cr exp lvl] returns the health points of the creature [cr]. If
    if the creature level[lvl] goes up because of the experience [exp] then the
    [cr]'s health points are increased. Otherwise, the [cr]'s health points stay
    the same. *)
let lvl_up_hp cr exp lvl =
  let curr_exp = exp/50 in
  if lvl <= curr_exp
  then cr.health_points + 10
  else cr.health_points

let add_exp cr exp = {
  name = cr.name;
  my_type = cr.my_type;
  attacks = cr.attacks;
  level = level_up cr (cr.experience + exp) cr.level;
  experience = cr.experience + exp;
  exp_for_lvl_up = rem_exp cr exp;
  health_points = lvl_up_hp cr (cr.experience + exp) cr.level;
  nickname = cr.nickname
}

(** [fire_type_cr attc_type pow] returns an integer represting how many
    health points will be lost based on the [attc_type] and [pow] when the cr
    is a fire type*)
let fire_type_cr attc_type pow =
  match attc_type with
  |Fire -> pow
  |Grass -> pow/2
  |Water -> 2*pow

(**[water_type_cr attc_type pow] returns an integer represting how many
   health points will be lost based on the [attc_type] and [pow] when the cr
   is a water type*)
let water_type_cr attc_type pow =
  match attc_type with
  |Fire -> pow/2
  |Grass -> 2*pow
  |Water -> pow

(**[grass_type_cr attc_type pow] returns an integer represting how many
   health points will be lost based on the [attc_type] and [pow] when the cr
   is a grass type*)
let grass_type_cr attc_type pow =
  match attc_type with
  |Fire -> 2*pow
  |Grass -> pow
  |Water -> pow/2

(**[type_damage cr tpe pow] returns an integer represting how many
   health points will be lost based on the [tpe], [pow] and the type of the
   [cr] being attacked.*)
let type_damage cr tpe pow =
  match creature_type cr with
  |Fire -> fire_type_cr tpe pow
  |Grass -> grass_type_cr tpe pow
  |Water -> water_type_cr tpe pow


let damage cr att =
  type_damage cr (attack_type att) att.power

let valid_attack at cr = 
  List.map attack_name cr.attacks |> List.mem at

(** [list_moves list] is a function that formats the list of attacks [list] into
    a string *)
let rec list_moves list =
  match list with
  | [] -> ""
  | l::[] -> "and \"" ^ (attack_name l) ^ "\""
  | h::t -> "\"" ^ (attack_name h) ^ "\", "  ^ list_moves t

let moves cr =
  match cr.attacks with
  | [] -> ""
  | l::[] -> "\"" ^ (attack_name l) ^ "\""
  | h::m::[] -> "\"" ^ (attack_name h) ^ "\" and \"" ^ (attack_name m) ^ "\""
  | h::t -> list_moves cr.attacks
