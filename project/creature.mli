(** Implements a functional data structure that stores creature information.
    Has functions to convert Json map data into OCaml data.*)

(** The creature's type. Affects the effectiveness of moves. *)
type ptype = Fire | Grass | Water

(** The abstract type representing an attack *)
type attack

(** The abstract type representing a creature. *)
type t

(** [c_from_json json] is the type t representation of [json]. *)
val c_from_json : Yojson.Basic.t -> t

(** [creature_name cr] is the name of the creature[cr]. *)
val creature_name : t -> string

(** [creature_type cr] is the ptype the creature[cr]. *)
val creature_type : t -> ptype

(** [creature_attcks cr] is the list of attacks of the creature[cr]. Each
    attack has a different ptype attached to it and attacks have different
    power levels.*)
val creature_attacks : t -> attack list

(** [creature_level cr] is the level of the creature[cr]. *)
val creature_level : t -> int

(** [creature_exp cr] is the amount of experience a creature[cr] has. *)
val creature_exp : t -> int

(** [creature_name cr] is the amount of experience a creature[cr] needs
    in total to level up [cr]. *)
val creature_exp_lvl_up : t -> int

(** [creature_max_hp cr] is the maximum amount of health points
    a creature[cr] has.*)
val creature_max_hp : t -> int

(** [creature_nickname cr] is the nickname of the creature [cr]. *)
val creature_nickname : t -> string

(** [set_nickname cr n] is a function that sets the creature's [cr] nickname to
    string [n] *)
val set_nickname : t -> string -> t

(** [attack_type att] is ptype of the attack[att]*)
val attack_type : attack -> ptype

(** [attack_name att] is name of the attack[att]*)
val attack_name : attack -> string

(** [attack_power att] is power of the attack[att]*)
val attack_power : attack -> int

(** [level_up cr exp lvl] returns the level [lvl] of the creature[cr]. If [exp]
    is greater than the experience needed to level up then the [lvl] is
    increased by one. Otherwise, the [lvl] stays the same.*)
val level_up : t -> int -> int -> int

(** [rem_exp cr exp] returns the remaining experience typically after a
    level up. If [exp] is greater than the experience needed to level up then
    [exp] needs to be reset by subtracting the experience needed to level up
    from [exp]. Otherwise, [exp] stays the same.*)
val rem_exp : t -> int -> int

(** [add_exp cr exp] is a function that adds experience points [exp] to the 
    creature [cr] and levels it up if necessary. *)
val add_exp : t -> int -> t

(** [damage cr att] returns how much damage [cr] takes from [att]
    based on the type of the creature[cr] and the power
    and type of the attack[att].*)
val damage : t -> attack -> int

(**[valid_attack at cr] returns true if attack [at] for creature [cr] and false
    otherwise*)
val valid_attack : string -> t -> bool

(** [moves cr] returns a string that lists the attacks creature [cr] has. *)
val moves : t -> string