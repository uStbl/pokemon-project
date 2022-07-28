(** 
   The main entry point for the game interface.
*)

(** [repl input st map] is a function that parses an input [input] into a 
    command and applies that command to the current state [st] in 
    map [map] and prompts the player for a new command on the new state.
    Exits the game with the input "quit". *)
val repl : string -> MapState.t -> BattleState.t -> Map.t -> PlayerState.t -> 
  unit

(** [to_string tup] is a function that takes in a tuple and returns a string
    representation of it *)
val tup_string : int * int -> string

(** [play_game f] starts the game in file [f]. *)
val play_game : string -> unit

(** [check_file ()] is a function that checks to see if a valid game file
    is entered. If the file is invalid, it prompts the user for a different 
    file. *)
val check_file : unit -> unit

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit

(** [dir_string dir] is a function that takes in a direction and returns a 
    string representation of that direction *)
val dir_string : MapCommand.direction -> string