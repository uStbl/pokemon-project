(** Implements a functional data structure that stores information
    about the current state of the player in relation to the map. *)

(** The abstract type of value representing the map state. *)
type t

(**Raised if move is Illegal *)
exception IllegalMove

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal

(** [init_state m] is the initial state of the map when playing map [m]. 
    In that state, the player is currently located in start position x and
    start position y. *)
val init_state : Map.t -> t

(** [position st] is the identity of the position the player is 
    currently on state [st]. *)
val position : t -> int * int

(**[match_direction dir st] returns a new position moved from state [st]
   according to the direction[dir]*)
val match_direction : MapCommand.direction -> t -> int*int

(**[set_position pos] is a function that sets the player's position to position
    [pos] *)
val set_position : int*int -> t

(** [move dir m st] returns a [result] of the player attempting to
    move with direction [dir] in state [st] and map [m].
    The function checks if the tile the player is attempting to move to is
    walkable. It is walkable then [result] is [Legal st'], where in [st'] the
    player is now located on the tile to which [dir] leads.
    Otherwise, the [result] is [Illegal].*)
val move : MapCommand.direction -> Map.t -> t -> PlayerState.t -> result

(**[get_state result] returns the state if move was Legal.
   Raises: IllegalMove if move was Illegal*)
val get_m_state : result -> t

(**[pick st cr] is the result of whether or not the player picked a valid 
    starter creature *)
val pick : t -> string -> result

(**[get_set cr lst p] returns the creature with name [cr] from the list [lst] of
    the player's [p] owned creatures.*)
val get_set : string -> Creature.t list -> PlayerState.t -> Creature.t option

(**[set st cr p] is the result of whether or not the player[p] set a valid 
    creature as their current creature*)
val set : t -> string -> PlayerState.t -> result
