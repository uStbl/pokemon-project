
type command =
  |Attack of string
  |Flee
  |Catch
  |Use of string
  |Items
  |Quit
  |Moves
  |Help

exception Malformed

exception Empty

let battle_parse event = 
  let index = if String.contains event ' ' then String.index event ' '
    else String.length event in
  let action = String.sub event 0 index in
  let move = String.sub event index (String.length event - index) in
  let trim = String.trim move in
  match action with
  |"attack" -> if trim <> "" then Attack trim
    else raise Malformed
  |"flee" -> if trim = "" then Flee else raise Malformed
  |"catch" -> if trim = "" then Catch else raise Malformed
  |"use" -> if trim <> "" then Use trim
    else raise Malformed
  |"items" -> if trim = "" then Items else raise Malformed
  |"quit" -> if trim = "" then Quit else raise Malformed
  |"moves" -> if trim = "" then Moves else raise Malformed
  |"help" -> if trim = "" then Help else raise Malformed
  |"" -> raise Empty
  |_ -> raise Malformed