(**
    Parsing of player commands outside of battle
*)

(*[direction] are the directions the player can move *)
type direction = 
  |North
  |South
  |East
  |West

(*[command] represents a player command that is a direction for map movement or
  quits the game *)
type command = 
  |Move of direction
  |Quit
  |Pick of string
  |Map
  |Items
  |Set of string
  |Creatures
  |Help

(*raised when a malformed command is encountered*)
exception Malformed

(*raised when an empty command in encountered*)
exception Empty

(**[parse event] is a function that parses a player's input and turns it into a 
                command. It takes in a string [event] and turns it into a 
                direction for the player to move.
                Raises: [Malformed] if [event] is not a valid movement or
                        [Empty] if [event] is an empty string or whitespace. *)
val parse : string -> command