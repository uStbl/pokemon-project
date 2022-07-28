(** Implements a functional data structure that stores information
    about the current state of a battle. *)

type t

type result = 
  |Legal of t
  |Illegal

(** Raised when the creature does not have any attacks *)
exception NoAttacks

(** Raised if change in state is Illegal *)
exception IllegalChange

(**[is_battle st] returns whether or not the player is in a battle. True if in 
    battle and false otherwise. *)
val is_battle : t -> bool

(**[get_battle_state st] returns the current battle state at statet [st]*)
val init_battle_state : t

(**[get_enemy st] is the enemy creature at battle state [st] *)
val get_enemy : t -> Creature.t

(**[get_creature st] is the player's current creature in battle state [st] *)
val get_curr_cr : t -> Creature .t

(**[get_curr_hp st] is the current hp of the player's creature at state [st]*)
val get_curr_hp : t -> int

(**[get_enemy_hp st] is the current hp of the enemy creature at state [st] *)
val get_enemy_hp : t -> int

(**[set_battle st v] sets the whether or not the player is currently in battle 
    at state [st] to value [v] *)
val set_battle : t -> bool -> t

(**[set_em_hp st v] sets the value of the enemy creature's hp at state [st] to 
    value [v]. *)
val set_em_hp : t -> int -> t

(**[set_curr_hp st v] sets the value of the player's creature's hp at state [st] 
    to value [v]. *)
val set_curr_hp : t -> int -> t

(**[set_battle_state b em cr em_hp hp] is battle state with battle value [b], 
    enemy creature [em], player creature [cr] and the enemy creature's hp is
    [em_hp] and the player's creature's hp is [hp]. *)
val set_battle_state : bool -> Creature.t option -> Creature.t option -> t

(**[rem_em_hp st at cr] is the health points of the enemy creature after
    it is hit with an attack from the current creature [cr] found from
    string [at] at state [st].
    Raise: [NoAttacks] if the [cr]'s attack list is empty or if the string [at]
    is not in the [cr]'s attack list*)
val rem_em_hp : t -> String.t -> Creature.t -> int

(**[get_attack r i lst] is the attack in the [lst] when the iterator [i]
    reaches int [r].
    Raise: [NoAttacks] if [lst] is empty or if [r] does not equal [i] after
    passing through all the elements of [lst]*)
val get_attack : int -> int -> Creature.attack list -> Creature.attack

(**[change_curr_hp cr st] is the health points of the player's current creature
    after it is hit with an attack from the enemy creature [em] at state [st].*)
val rem_curr_hp : t -> int -> Creature.attack list -> int

(**[add_curr_hp cr st it] is the health points of the player's creature [cr]
    after an item [it] is used on it at state [st]. *)
val add_curr_hp : Creature.t -> t -> string -> int

(**[attack st cr at] is the result of trying to attack with creature [cr] using 
    attack [at] at state [st] *)
val attack : t -> Creature.t -> string -> result

(**[catch st em_cr player] is the result if [player] attempts to catch the
    creature [em_cr] at state [st]. *)
val catch : t -> Creature.t -> PlayerState.t -> result

(**[flee st] is the result of trying to flee at state [st] *)
val flee : t -> result

(**[use st p it] is the result of player [p] trying to use item [it] at state
    [st] *)
val use : t -> PlayerState.t -> string -> result

(**[get_bst result] returns the state if change in battle state was Legal.
   Raises: IllegalChange if the change was Illegal*)
val get_bst : result -> t