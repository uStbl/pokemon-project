(**
    Handles parsing of player inputs into commands during battle.
*)

(*[command] represents a player command during battle *)
type command =
  |Attack of string
  |Flee
  |Catch
  |Use of string
  |Items
  |Quit
  |Moves
  |Help

(*raised when a malformed command is encountered*)
exception Malformed

(*raised when an empty command in encountered*)
exception Empty

(**[battle_parse event] is a function that parses a player's input and turns it 
                into a command. It takes in a string [event] and turns it into 
                an action performed by the player while in battle.
                Raises: [Malformed] if [event] is not a valid action or
                        [Empty] if [event] is an empty string or whitespace. *)
val battle_parse : string -> command