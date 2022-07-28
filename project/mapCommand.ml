
type direction = 
  |North
  |South
  |East
  |West

type command = 
  |Move of direction
  |Quit
  |Pick of string
  |Map
  |Items
  |Set of string
  |Creatures
  |Help

exception Malformed

exception Empty

let parse event = 
  let index = if String.contains event ' ' then String.index event ' '
    else String.length event in
  let action = String.sub event 0 index in
  let move = String.sub event index (String.length event - index) in
  let trim = String.trim move in
  match action with
  |"up" -> if trim = "" then Move North else raise Malformed
  |"left" ->  if trim = "" then Move West else raise Malformed
  |"down" ->  if trim = "" then Move South else raise Malformed
  |"right" ->  if trim = "" then Move East else raise Malformed
  |"quit" ->  if trim = "" then Quit else raise Malformed
  |"pick" -> if trim <> "" then Pick trim else raise Malformed
  |"map" ->  if trim = "" then Map else raise Malformed
  |"items" -> if trim = "" then Items else raise Malformed
  |"set" -> if trim <> "" then Set trim else raise Malformed
  |"creatures" -> if trim = "" then Creatures else raise Malformed
  |"help" -> if trim = "" then Help else raise Malformed
  |"" -> raise Empty
  |_ -> raise Malformed