open Map
open MapCommand
open MapState
open Creature
open BattleCommand
open BattleState
open PlayerState
open Printf

(** [map_list lst] goes through the [lst] and prints the characters to form
    a map.*)
let rec map_list lst = 
  let map_print lst = 
    match lst with
    |list -> List.iter (printf "%c   ") list in
  match lst with
  |h::t -> map_print h; print_string "\n\n"; map_list t
  |_ -> ()

(** [dir_string dir] is a function that takes in a direction and returns a 
    string representation of that direction *)
let dir_string dir = 
  match dir with
  |North -> "North"
  |South -> "South"
  |East -> "East"
  |West -> "West"

(** [to_string tup] is a function that takes in a tuple and returns a string
    representation of it *)
let tup_string tup = 
  match tup with
  |(h,t) -> "(" ^ (string_of_int h) ^ ", " ^ (string_of_int t) ^ ")"

(** [continue r] is a function that reads in an input and returns true if it is 
    "yes" and false if "no". If the response is not valid, it prompts the user
    for another response. *)
let rec continue r =
  match r with
  |"yes" -> true
  |"no" -> false
  |_ -> print_endline "That is not a valid answer.";
    print_string "> ";
    continue (read_line())

(** [used v lst] is a function that returns true if the nickname has not been
    used yet and false otherwise. *)
let rec used v lst =
  match lst with
  |[] -> false
  |h::t -> if (creature_nickname h) = v then true
    else used v t

(**[valid_nickname n] is a function that checks whether or not nickname [n] is
    valid. *)
let rec valid_nickname n p =
  let trim = String.trim n in
  match trim with
  |"" -> false
  |v -> not (used v (owned_creatures p))

(**[nickname n] returns the nickname for the creature. *)
let rec nickname n p =
  if (valid_nickname n p) then n
  else (print_endline "That is not a valid nickname.";
        print_string "> ";
        nickname (read_line()) p)

(**[help_map] is a function that prints a list of commands the player can use
    when they are not in battl *)
let help_map = "List of commands:\n" ^
               "map -> show the state of the map\n" ^
               "set <c> -> set <c> as the creature you want " ^
               "to battle with\n" ^
               "pick <c> -> pick <c> as your starter creature\n" ^
               "up -> move up one space\n" ^
               "down -> move down one space\n" ^
               "left -> move left one space\n" ^
               "right -> move right one space\n" ^
               "items -> show the items you own\n" ^
               "creatures -> show the creatures you own\n" ^
               "quit -> quit the game"

(**[help_bat] is a function that prints a list of commands that the player can
    use when they are in battle. *)
let help_bat = "List of commands:\n" ^
               "attack <m> -> attack the enemy creature with the move named " ^
               "<m>\n" ^
               "catch -> catch the enemy creature if you have a catch ball\n" ^
               "flee -> flee from the battle\n" ^
               "use <i> -> use the item named <i>\n" ^
               "items -> show the items you own\n" ^
               "moves -> show the moves your creature can use\n" ^
               "quit -> quit the game"

(** [opp_faint p bst1] is a function that returns the updated player state from
    the previous player state [p] and battle state [bst1] after the enemy 
    creature has fainted. *)
let opp_faint p bst1 =
  let c = add_exp (get_cr (get_curr p)) 20 in
  let i = rem_cr p (creature_nickname (get_curr_cr bst1))
      (owned_creatures p) in
  let p1 = set_inv p i in
  let p2 = add_creature p1 c in
  set_curr p2 (Some c)

(** [caught p c' bst1] is a function that returns the updated player state from
    the previous player state [p], updated creature [c'], and battle state 
    [bst1] when a creature is caught. *)
let caught p c' bst1 =
  let p1 = add_creature p c' in
  let it = rem_item p1 "catchball" (get_items p1) in
  let p2 = set_items p1 it in
  let c = add_exp (get_cr (get_curr p2)) 10 in
  let i = rem_cr p2 (creature_nickname (get_curr_cr bst1))
      (owned_creatures p2) in
  let p3 = set_inv p2 i in
  let p4 = add_creature p3 c in
  set_curr p4 (Some c)

(** [set_helper st cr p] is the updated player state from the previous player
    state [p] to set the current creature to the creature with the
    corresponding nickname [cr] when the player is in the map state [st]. *)
let set_helper st cr p =
  let res = set st cr p in
  match res with
  |Legal (state) ->
    print_endline ("You set " ^ cr ^ 
                   " as your current creature.");
    set_curr p (get_set cr (owned_creatures p) p)
  |Illegal -> print_endline 
                "You cannot set that creature as your current creature."; p

(** [pick_helper st cr p map] is the updated player state from the previous
    player state [p] to pick a creature with the corresponding name [cr] when
    the player is in the map state [st] in map [map]. It also gives the player
    items when they successfully pick.*)
let pick_helper st cr p map =
  let res = pick st cr in 
  match res with
  |Legal (state) -> if List.length (owned_creatures p) = 0 
    then (let c = pick_creature map cr in
          print_endline ("You picked " ^ cr);
          print_endline("Please give your creature a nickname.");
          print_string "> ";
          let n = nickname (read_line()) p in
          let c' = set_nickname c n in
          print_endline ("You set " ^ cr ^ "'s nickname to " ^ n ^ ".");
          let p1 = add_creature p c' in
          let p2 = set_curr p1 (Some c') in
          set_items p2 (["potion"; "potion"; "catchball"; "catchball"; 
                         "catchball"; "catchball"]))
    else (print_endline "You already have a creature."; p)
  |Illegal -> (print_endline "You cannot pick that creature."; p)

(** [enemy_attack state bst1] is a function that returns the updated battle 
    state using the battle state [bst1] after an enemy creature attacks. *)  
let enemy_attack bst1 =
  let lst = creature_attacks (get_enemy bst1) in
  let r = Random.int (List.length lst) in
  let bst2 = set_curr_hp bst1 (rem_curr_hp bst1 r lst) in
  let att = attack_name (get_attack r 0 lst) in
  print_endline ("The opposing " ^ creature_name (get_enemy bst1) ^ 
                 " used " ^ att);
  print_endline ("Your " ^ creature_nickname (get_curr_cr bst1) ^ " has " ^ 
                 string_of_int (get_curr_hp bst2) ^ " hp.");
  if (get_curr_hp bst2) <= 0 then
    let bst3 = set_battle bst2 false in
    print_endline "Your creature fainted!";
    bst3
  else
    bst2

(** [flee bst] is a function that matches the current battle state [bst] with a
    result and returns the appropriate battle state. *)
let flee_helper bst = 
  let res = flee bst in 
  match res with
  |Legal (state) -> (print_endline "You ran away!"; state)
  |Illegal -> (print_endline "You cannot run away."; bst)

(** [use_helper bst p it] is the updated player state when the player [p] 
     uses an item [it] in battle state [bst]. *)
let use_helper bst p it = 
  print_endline ("You used a " ^ it);
  print_endline (creature_nickname (get_curr_cr bst) ^ " now has " ^
                 string_of_int (get_curr_hp bst) ^ " hp.");
  set_items p (rem_item p it (get_items p))


(** [catch_helper state] is the creature caught by 
    player [p] in battle state [state]. *)
let catch_helper state p =
  print_endline ("You caught " ^ (creature_name (get_enemy state)) ^ "!");
  print_endline ("Your creature gained 10 exp.");
  print_endline ("Please give your creature a nickname.");
  print_string "> ";
  let n = nickname (read_line()) p in
  let c' = set_nickname (get_enemy state) n in
  let cr = state |> get_enemy |> creature_name in
  print_endline ("You set " ^ cr ^ "'s nickname to " ^ n ^ ".");
  c'

(** [repl input st map] is a function that parses an input [input] into a 
    command and applies that command to the current state [st] in 
    map [map] and prompts the player for a new command on the new state.
    Exits the game with the input "quit". *)
let rec repl input st bst map p =
  if not (is_battle bst) then (
    try match (parse input) with
      |Quit -> print_endline "Goodbye! Thanks for playing!"; exit 0
      |Map -> let view = viewable_from_position map (position st) in
        map_list view;
        print_string "> ";
        repl (read_line()) st bst map p
      |Set (cr) -> (let p1 = set_helper st cr p in
                    print_string "> ";
                    repl (read_line()) st bst map p1)
      |Pick (cr) -> let p' = pick_helper st cr p map in
        print_string "> ";
        repl (read_line()) st bst map p'
      |Move (dir) -> (let res = move dir map st p in
                      match res with
                      |Legal (st') ->
                        print_endline ("You have moved " ^ (dir_string dir));
                        let view = viewable_from_position map
                            (position st') in
                        map_list view;
                        let c = encounter map (position st') 
                            (all_creatures map) in
                        let i = item_enc map (position st') (map_items map) in
                        if c <> None then let bst' = set_battle_state true c 
                                              (get_curr p) in
                          print_endline ("You have encountered a wild " ^ 
                                         creature_name (get_enemy bst'));
                          print_string "> ";
                          repl (read_line()) st' bst' map p
                        else if i <> None then let it = get_it i in
                          let p' = add_item p it in
                          print_endline ("You have found a " ^ it ^ "!");
                          print_string "> ";
                          repl (read_line()) st' bst map p'
                        else
                          print_string "> ";
                        repl (read_line()) st' bst map p
                      |Illegal ->
                        if List.length (owned_creatures p) = 0 then 
                          (print_endline "You must pick a creature first.";
                           print_string "> ";
                           repl (read_line()) st bst map p)
                        else
                          print_endline "You cannot perform that action.";
                        print_string "> ";
                        repl (read_line()) st bst map p)
      |Items -> (if items p = "" then 
                   print_endline ("You do not have any items.")
                 else 
                   print_endline ("You have a " ^ (items p) ^ 
                                  " in your inventory.");
                 print_string "> ";
                 repl (read_line ()) st bst map p)
      |Creatures -> (if creatures p = "" then 
                       print_endline ("You do not have any creatures.")
                     else
                       print_endline ("You have " ^ (creatures p) ^ 
                                      " currently.");
                     print_string "> ";
                     repl (read_line()) st bst map p)
      |Help ->  print_endline help_map;
        print_string "> ";
        repl (read_line()) st bst map p
    with
    |MapCommand.Malformed ->
      print_endline ("That is not a valid command. Type \"help\" for a list of "
                     ^ "valid commands.");
      print_string "> ";
      repl (read_line()) st bst map p
    |MapCommand.Empty -> print_endline "Please enter a command.";
      print_string "> ";
      repl (read_line ()) st bst map p
    |Map.InvalidTile -> print_endline "That is not a valid movement.";
      print_string "> ";
      repl (read_line()) st bst map p
    |Map.NoCreature -> print_endline "That creature does not exist.";
      print_string "> ";
      repl (read_line()) st bst map p
    |Map.NoItem -> print_endline "That item does not exist.";
      print_string "> ";
      repl (read_line()) st bst map p
    |MapState.IllegalMove -> print_endline "That movement is not possible.";
      print_string "> ";
      repl (read_line()) st bst map p
    |PlayerState.NoItems -> print_endline "You have no catchballs.";
      print_string "> ";
      repl (read_line()) st bst map p
    |BattleState.NoAttacks -> print_endline "This creature has no attacks";
      print_string "> ";
      repl (read_line()) st bst map p
    |BattleState.IllegalChange -> print_endline "Invalid battle state.";
      print_string "> ";
      repl (read_line()) st bst map p
  )
  else (
    try match (battle_parse input) with
      |Quit-> print_endline "Goodbye! Thanks for playing!"; exit 0
      |Attack (mv) -> (let res = attack bst (get_curr_cr bst) mv in
                       match res with
                       |Legal (state) -> (
                           print_endline (creature_nickname (get_curr_cr state)
                                          ^ " used " ^ mv);
                           print_endline ("The opposing " ^ creature_name 
                                            (get_enemy state) ^ " has " ^ 
                                          string_of_int (get_enemy_hp state) 
                                          ^ " hp.");
                           if (get_enemy_hp state>0) && (get_curr_hp state>0)
                           then
                             let bst2 = enemy_attack state in
                             print_string "> ";
                             repl (read_line()) st bst2 map p
                           else 
                             let p' = opp_faint p state in
                             let bst3 = set_battle state false in
                             (print_endline "The opposing creature fainted!";
                              print_endline "Your creature gained 20 exp.";
                              print_string "> ";
                              repl (read_line()) st bst3 map p'))
                       |Illegal -> 
                         print_endline "You cannot use that attack.";
                         print_string "> ";
                         repl (read_line()) st bst map p)
      |Flee -> (let bst' = flee_helper bst in
                print_string "> ";
                repl (read_line ()) st bst' map p)
      |Catch -> (let res = catch bst (get_enemy bst) p in 
                 match res with
                 |Legal (state) -> (
                     let c' = catch_helper state p in
                     let bst1 = set_battle state false in 
                     let p' = caught p c' bst1 in
                     if (won p') && not (get_win p') then (
                       print_endline "Congratulations! You have won!";
                       print_endline "Would you like to continue playing?";
                       print_string "> ";
                       if continue (read_line()) then 
                         let p1 = set_win p' true in
                         print_string "> ";
                         repl (read_line()) st bst1 map p1
                       else print_endline "Thanks for playing!"; exit 0)
                     else print_string "> ";
                     repl (read_line ()) st bst1 map p')
                 |Illegal -> let it = rem_item p "catchball" (get_items p) in
                   let p' = set_items p it in
                   print_endline ("You did not catch " ^ 
                                  (creature_name (get_enemy bst)));
                   print_string "> ";
                   repl (read_line ()) st bst map p')
      |Use (it) -> (let res = use bst p it in
                    match res with
                    |Legal (state) -> let p' = use_helper state p it in
                      print_string "> ";
                      repl (read_line ()) st state map p'
                    |Illegal -> print_endline "You cannot use that item.";
                      print_string "> ";
                      repl (read_line()) st bst map p)
      |Items -> (if items p = "" then 
                   print_endline ("You do not have any items.")
                 else 
                   print_endline ("You have a " ^ (items p) 
                                  ^ " in your inventory.");
                 print_string "> ";
                 repl (read_line ()) st bst map p)
      |Moves -> (if moves (get_curr_cr bst) = "" then
                   print_endline ("This creature does not have any moves.")
                 else
                   print_endline ("This creature has " ^ 
                                  (moves (get_curr_cr bst)) ^ " currently.");
                 print_string "> ";
                 repl (read_line()) st bst map p)
      |Help -> print_endline help_bat;
        print_string "> ";
        repl (read_line()) st bst map p
    with
    |BattleCommand.Malformed ->
      print_endline ("That is not a valid command. Type \"help\" for a list of "
                     ^ "valid commands.");
      print_string "> ";
      repl (read_line()) st bst map p
    |BattleCommand.Empty -> print_endline "Please enter a command.";
      print_string "> ";
      repl (read_line ()) st bst map p
    |Map.InvalidTile -> print_endline "That is not a valid movement.";
      print_string "> ";
      repl (read_line()) st bst map p
    |Map.NoCreature -> print_endline "That creature does not exist.";
      print_string "> ";
      repl (read_line()) st bst map p
    |Map.NoItem -> print_endline "That item does not exist.";
      print_string "> ";
      repl (read_line()) st bst map p
    |MapState.IllegalMove -> print_endline "That movement is not possible.";
      print_string "> ";
      repl (read_line()) st bst map p
    |PlayerState.NoItems -> print_endline "You have no catchballs.";
      print_string "> ";
      repl (read_line()) st bst map p
    |BattleState.NoAttacks -> print_endline "This creature has no attacks";
      print_string "> ";
      repl (read_line()) st bst map p
    |BattleState.IllegalChange -> print_endline "Invalid battle state.";
      print_string "> ";
      repl (read_line()) st bst map p)

(** [play_game f] starts the game in file [f]. *)
let play_game f =
  let file = Yojson.Basic.from_file f in
  let map = Map.from_json file in
  let current = init_state map in
  let bst = init_battle_state in
  let player = init_player in
  let view = viewable_from_position map (position current) in
  print_endline ("You have begun your adventure! "
                 ^ "Collect a lucky number of creatures to win the game!");
  map_list view;
  print_endline "To help you along your journey, choose a starter creature.";
  print_endline "You can pick either Squirtel, Bulbasoar, or Tarmander.";
  print_string "> ";
  match read_line () with
  |exception End_of_file -> ()
  |input -> repl input current bst map player

(** [check_file ()] is a function that checks to see if a valid game file
    is entered. If the file is invalid, it prompts the user for a different 
    file. *)
let rec check_file () =
  try match read_line () with
    |exception End_of_file -> ()
    |file_name -> play_game file_name
  with
  |Yojson.Json_error (arg) -> print_endline "Please enter a valid game file.";
    print_string "> ";
    check_file ()
  |Sys_error (arg) -> print_endline "Please enter a valid game file.";
    print_string "> ";
    check_file ()
  |Yojson.Basic.Util.Type_error (arg1, arg2) -> 
    print_endline "Please enter a valid game file.";
    print_endline "> ";
    check_file ()

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  check_file ()

(* Execute the game engine. *)
let () = main ()