
(** Implements a functional data structure that stores map information.
    Has functions to convert Json map data into OCaml data. *)

(** The abstract type representing the map. *)
type t

(** The type representing map tiles. Each tile has a different character
    representation and may have different properties. *)
type tile = Path | Tree | Grass

(** The type representing a map position. The position x, y is the tile located
    x tiles right and y tiles down from the top-left tile. *)
type coord = int * int

(** Raised if character is not a valid tile. *)
exception InvalidTile

(** Raised if there is no such creature in the map. *)
exception NoCreature

(** Raised if there is no such item in the map. *)
exception NoItem

(** [from_json j] is the type t representation of [j]. *)
val from_json : Yojson.Basic.t -> t

(** [viewable_from_position map a] is the 2D list containing every tile
    viewable from position [a]. *)
val viewable_from_position : t -> coord -> char list list

(** [char_at_position map pos] is the character on the map [map] at position
    [pos]. *)
val char_at_position : t -> coord -> char

(** [tile_at_position map a] is the tile at position [a]. *)
val tile_at_position : t -> coord -> tile

(** [initial_position map] is the initial position of map [map]. *)
val initial_position : t -> int * int

(** [map_items map] is the list of all items in that appear in map [map] *)
val map_items : t -> string list

(** [all_creatures map] is a list of all the creatures that appear
    in map [map]. *)
val all_creatures : t -> Creature.t list

(** [pick_creature map name] is the creature with name [name] in map [map].
    Raises: NoCreature if there is no creature with name [name] in the map. *)
val pick_creature : t -> string -> Creature.t

(** [legal_position map a] is true if position [a] is on map [map]. *)
val legal_position : t -> coord -> bool

(** [encounter m pos cr_list] returns a creature option from the [cr_list]
    if the player's [pos] is on a grass tile in map [m].*)
val encounter: t -> coord -> Creature.t list -> Creature.t option

(** [get_cr cr] is the value of the creature option [cr].
    Raises: [NoCreature] if creature option [cr] is None. *)
val get_cr : Creature.t option -> Creature.t

(**[item_enc m pos it_list] returns an item from the item list [it_list]
    if the player's position [pos] is on a path tile in map [m] *)
val item_enc : t -> coord -> string list -> string option


(**[get_it it] is the value of the string option [it]
   Raises: [NoItem] if string option [it] is None.*)
val get_it : string option -> string