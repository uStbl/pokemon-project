(** Implements a functional data structure that stores information
    about the creatures and items the player owns. *)

(** The abstract type of value representing the player state. *)
type t

(** Raised when the player does not have any items to remove *)
exception NoItems

(** [init_player st] is the current state of the player at state [st] *)
val init_player : t

(** [get_items p] is a string list containing all the creatures player [p]
    owns. *)
val get_items : t -> string list

(** [get_curr player] is the player's [player] current creature*)
val get_curr : t -> Creature.t option

(**[get_win player] is the player's current winning status *)
val get_win : t -> bool

(** [set_items p it] sets the list of the player's [p] items to list [it]*)
val set_items : t -> string list -> t

(** [set_inventory player inv] sets the list of the player's [p] owned creatures
    to list [inv].*)
val set_inv: t -> Creature.t list -> t

(** [add_item player it] is a function that adds item [it] to the player's
    [player] list of items *)
val add_item : t -> string -> t

(** [set_curr player cr] is a function that sets the player's [player] current
    creature to creature [cr] *)
val set_curr : t -> Creature.t option -> t

(** [set_win player w] is a function that sets the player's [player] win status
    to a boolean [w] *)
val set_win : t -> bool -> t

(** [owned_creatures p] is a list containing all the creatures
    player [p] owns. *)
val owned_creatures : t -> Creature.t list

(** [add_creature p c] is the state containing every creature in state [p]
    plus creature [c]. *)
val add_creature : t -> Creature.t -> t

(** [rem_item p it lst] is the remaining items that a player [p] has after an
    item [it] is removed from the list of items [lst] *)
val rem_item : t -> string -> string list -> string list

(** [rem_cr p cr lst] is the remaining creatures that a player [p] has after
    removing creature [cr] from the list of owned creatures [lst]. *)
val rem_cr : t -> string -> Creature.t list -> Creature.t list

(** [items player] is a string of items the player [p] has. *)
val items : t -> string

(** [creatures player] is a string of creatures the player [player] has. *)
val creatures : t -> string

(** [won p] returns true if the player [p] has met the winning condition *)
val won : t -> bool