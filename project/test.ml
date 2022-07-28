(** We utilized both black box and glass box testing to derive our test cases. 
    We tested functions from the [Map], [MapCommand], [MapState],
    [BattleCommand], [BattleState], [PlayerState] and [Creature] modules 
    directly through OUnit. The functions in the [Creature] module are also used 
    by many of the other modules, so they were indirecly tested by testing those 
    modules in OUnit. We were able to test many of the getter and setter 
    functions in these modules. However, many of the functions in [MapState], 
    [BattleState], and [PlayerState] could not be tested automatically through 
    OUnit because the states were altered in main. In addition, some of the 
    functions utilized random integers, so it was impossible to test because the
    output would vary depending on the random integer that was generated in the
    function. Therefore, in order to ensure that the states and random integers
    were being updated and generated correctly, we ran [make play] several 
    times and tested all of the commands with various inputs to ensure the 
    variables were updating correctly and the game did not crash. We believe 
    that the combination of our OUnit test cases along with our multiple test 
    runs using [make play] is sufficient enough to demonstrate the correctness 
    of our system. Running [make play] was the most effective way to catch bugs 
    because we were able to ensure that the terminal was printing out the 
    correct information and that the commands were correctly updating various 
    states. *)

open OUnit2
open Map
open MapCommand
open MapState
open BattleCommand
open BattleState
open PlayerState
open Creature

let main_map = "game_map.json" |> Yojson.Basic.from_file |> from_json

let map_tests = [
  "test viewable" >:: (fun _ -> assert_equal
                          [
                            ['!';'!';'!';'!';'!'];
                            ['!';'#';'#';'.';'.'];
                            ['!';'#';'#';'.';'.'];
                            ['!';'#';'#';'.';'.'];
                            ['!';'.';'.';'.';'.'];
                          ]
                          (viewable_from_position main_map (0, 0)));
  "test viewable 2" >:: (fun _ -> assert_equal
                            [
                              ['.';'.';'.';'.';'.'];
                              ['#';'.';'.';'.';'.'];
                              ['#';'.';'.';'.';'.'];
                              ['#';'.';'.';'.';'.'];
                              ['.';'.';'.';'.';'.'];
                            ]
                            (viewable_from_position main_map (7, 7)));
  "test tile at" >:: (fun _ -> assert_equal Map.Grass
                         (tile_at_position main_map (5,8)));
  "test tile at 2" >:: (fun _ -> assert_equal Map.Path 
                           (tile_at_position main_map (7,7)));
  "test tile at 3" >:: (fun _ -> assert_equal Map.Tree
                           (tile_at_position main_map (0,0)));
  "test init pos" >:: (fun _ -> assert_equal (7,7)
                          (initial_position main_map));
  "test char at" >:: (fun _ -> assert_equal '!' 
                         (char_at_position main_map (0, 0)));
]

let mapCommand_tests = [
  "test parse 1" >:: (fun _ -> assert_equal (Move North) (parse "up"));
  "test parse 2" >:: (fun _ -> assert_equal (Move South) (parse "down"));
  "test parse 3" >:: (fun _ -> assert_equal (Move East) (parse "right"));
  "test parse 4" >:: (fun _ -> assert_equal (Move West) (parse "left"));
  "test parse 5" >:: (fun _ -> assert_raises MapCommand.Empty 
                         (fun() -> parse ""));
  "test parse 6" >:: (fun _ -> assert_raises MapCommand.Malformed 
                         (fun() -> parse "UP"));
  "test parse 7" >:: (fun _ -> assert_raises MapCommand.Malformed 
                         (fun() -> parse "3"));
  "test parse 8" >:: (fun _ -> assert_raises MapCommand.Malformed 
                         (fun() -> parse "="));
  "test parse 9" >:: (fun _ -> assert_raises MapCommand.Malformed 
                         (fun() -> parse "\n"));
  "test parse 10" >:: (fun _ -> assert_raises MapCommand.Malformed 
                          (fun() -> parse "\t"));
  "test parse 11" >:: (fun _ -> assert_raises MapCommand.Malformed 
                          (fun() -> parse "pick "));
  "test parse 12" >:: (fun _ -> assert_equal Map (parse "map"));
  "test parse 13" >:: (fun _ -> assert_equal MapCommand.Quit (parse "quit"));
  "test parse 14" >:: (fun _ -> assert_equal (Pick "Squirtel")
                          (parse "pick Squirtel"));
  "test parse 15" >:: (fun _ -> assert_equal (Pick "w") (parse "pick w"));
  "test parse 16" >:: (fun _ -> assert_raises MapCommand.Malformed 
                          (fun() -> parse "pick  "));
  "test parse 17" >:: (fun _ -> assert_raises MapCommand.Malformed 
                          (fun() -> parse "quit lkjlk "));
  "test parse 18" >:: (fun _ -> assert_raises MapCommand.Malformed 
                          (fun() -> parse "map hi"));
  "test parse 19" >:: (fun _ -> assert_raises MapCommand.Malformed 
                          (fun() -> parse "up hi"));
  "test parse 20" >:: (fun _ -> assert_raises MapCommand.Malformed 
                          (fun() -> parse "down hi"));
  "test parse 21" >:: (fun _ -> assert_raises MapCommand.Malformed 
                          (fun() -> parse "left hi"));
  "test parse 22" >:: (fun _ -> assert_raises MapCommand.Malformed 
                          (fun() -> parse "right hi"));
  "test parse 23" >:: (fun _ -> assert_equal (Set "Squirtel")
                          (parse "set Squirtel"));
  "test parse 24" >:: (fun _ -> assert_equal MapCommand.Help (parse "help"));
]
let cr_sq = pick_creature main_map "Squirtel"
let in_state = init_state main_map
let pi = init_player
let p = add_creature pi cr_sq
let tarmander = pick_creature main_map "Tarmander"
let bulbasoar = pick_creature main_map "Bulbasoar"
let squirtel_named = set_nickname cr_sq "John Squirtel"
let state1 = get_m_state(move North main_map in_state p)
let state2 = get_m_state(move East main_map state1 p)
let state3 = get_m_state(move West main_map state2 p)
let state4 = get_m_state(move South main_map state3 p)
let state5 = get_m_state(move North main_map state1 p)
let state6 = get_m_state(move North main_map state5 p)
let state7 = get_m_state(move North main_map state6 p)
let state8 = get_m_state(move North main_map state7 p)
let state9 = get_m_state(move North main_map state8 p)
let state10 = get_m_state(pick in_state "Tarmander")
let state_res = pick in_state "Charmander"
let state11 = get_m_state(set state5 "" p)
let p_nn = add_creature pi squirtel_named
let get = get_set "John Squirtel" (owned_creatures p_nn) p_nn
let get2 = get_set "John Tarmander" (owned_creatures p) p
let set_pos = set_position (3,4)

let mapState_tests = [
  "test init_state" >::(fun _ ->
      assert_equal ((7,7)) (position (in_state)));
  "test Illegal move" >:: (fun _ -> assert_raises (IllegalMove)
                              (fun () ->
                                 get_m_state(move North main_map state9 p)));
  "test move N" >:: (fun _ -> assert_equal (7,6) (position state1));
  "test move E" >:: (fun _ -> assert_equal (8,6) (position state2));
  "test move W" >:: (fun _ -> assert_equal (7,6) (position state3));
  "test move S" >:: (fun _ -> assert_equal (7,7) (position state4)); 
  "test pick" >:: (fun _ -> assert_equal ((7,7)) (position state10));
  "test invalid pick" >:: (fun _ -> assert_raises (IllegalMove)
                              (fun () ->
                                 get_m_state(state_res)));
  "test set" >:: (fun _ -> assert_equal ((7,5)) (position state11));
  "test invalid set" >:: (fun _ -> assert_raises (IllegalMove)
                             (fun () ->
                                get_m_state(set in_state "Tarmander" p)));
  "test get_set 1" >:: (fun _ -> assert_equal "Squirtel"
                           (creature_name(get_cr get)));
  "test get_set 2" >:: (fun _ -> assert_equal (None) (get2));
  "test set_position" >:: (fun _ -> assert_equal (3,4) (position (set_pos)));
]

let battleCommand_tests = [
  "test battle_parse 1" >:: (fun _ -> assert_equal (Attack "Water Gun") 
                                (battle_parse "attack Water Gun"));
  "test battle_parse 2" >:: (fun _ -> assert_equal (Attack "w")
                                (battle_parse "attack w "));
  "test battle_parse 3" >:: (fun _ -> assert_equal Flee (battle_parse "flee"));
  "test battle_parse 4" >:: (fun _ -> assert_equal Catch 
                                (battle_parse "catch"));
  "test battle_parse 5" >:: (fun _ -> assert_equal (Use "potion") 
                                (battle_parse "use potion"));
  "test battle_parse 6" >:: (fun _ -> assert_equal Items 
                                (battle_parse "items"));
  "test battle_parse 7" >:: (fun _ -> assert_raises BattleCommand.Malformed
                                (fun() -> battle_parse "attack  "));
  "test battle_parse 8" >:: (fun _ -> assert_raises BattleCommand.Empty
                                (fun() -> battle_parse ""));
  "test battle_parse 9" >:: (fun _ -> assert_raises BattleCommand.Malformed
                                (fun() -> battle_parse "flee help"));
  "test battle_parse 10" >:: (fun _ -> assert_raises BattleCommand.Malformed
                                 (fun() -> battle_parse ";lajf09u30q"));
  "test battle_parse 11" >:: (fun _ -> assert_raises BattleCommand.Empty
                                 (fun() -> battle_parse "       "));
  "test battle_parse 12" >:: (fun _ -> assert_equal BattleCommand.Quit 
                                 (battle_parse "quit"));
  "test battle_parse 13" >:: (fun _ -> assert_equal BattleCommand.Help
                                 (battle_parse "help"));
  "test battle_parse 14" >:: (fun _ -> assert_equal Moves 
                                 (battle_parse "moves"));
]

let bst = init_battle_state
let p1 = set_curr p (Some cr_sq)
let p2 = add_item p1 "potion"
let p3 = add_item p2 "catchball"
let p4 = add_creature p2 bulbasoar
let bst1 = set_battle_state true (Some bulbasoar) (get_curr p1)
let res1 = attack bst1 (get_curr_cr bst1) "Water Gun"
let res2 = attack bst1 (get_curr_cr bst1) "Leaf Throw"
let bst1' = get_bst res1
let res3 = catch bst1' (get_enemy bst1') p3
let bst3 = set_em_hp bst1' 15
let res4 = catch bst3 (get_enemy bst3) p3
let bst4 = get_bst res4
let bst5 = set_curr_hp bst3 10
let bst6 = rem_curr_hp bst3 0 (creature_attacks (get_enemy bst3))
let bst7 = rem_curr_hp bst5 2 (creature_attacks (get_enemy bst5))

let battleState_tests = [
  "init_bst" >:: (fun _ -> assert_equal false (is_battle bst));
  "test set_battle true" >:: (fun _ -> assert_equal true
                                 (true |> set_battle bst |> is_battle));
  "test set_battle false" >:: (fun _ -> assert_equal false 
                                  (false |> set_battle bst |> is_battle));
  "test set_em_hp" >::(fun _ -> assert_equal 15 (get_enemy_hp bst3));
  "test set_curr_hp" >::(fun _ -> assert_equal 10 (get_curr_hp bst5));
  "set_battle_state 1" >:: (fun _ -> assert_equal true
                               (is_battle bst1));
  "set_battle_state 2" >:: (fun _ -> assert_equal "Bulbasoar"
                               (creature_name(get_enemy bst1)));
  "set_battle_state 3" >:: (fun _ -> assert_equal "Squirtel"
                               (creature_name(get_curr_cr bst1)));
  "set_battle_state 4" >:: (fun _ -> assert_equal 30 (get_enemy_hp bst1));
  "set_battle_state 5" >:: (fun _ -> assert_equal 30 (get_curr_hp bst1));
  "use potion at max hp" >:: (fun _ -> assert_equal 30 
                                 ("potion" |> use bst1 p2 
                                  |> get_bst |> get_curr_hp));
  "use potion not at max hp" >:: (fun _ -> assert_equal 20
                                     ("potion"|> use bst5 p2
                                      |> get_bst |> get_curr_hp));
  "invalid attack" >:: (fun _ -> assert_raises IllegalChange
                           (fun() -> get_bst(res2)));
  "rem_curr_hp 1" >:: (fun _ -> assert_equal 10 (bst6));
  "rem_curr_hp 2" >:: (fun _ -> assert_equal 0 (bst7));
  "diff hp after attack" >:: (fun _ -> assert_equal 25 
                                 (get_enemy_hp bst1'));
  "invalid catch" >:: (fun _ -> assert_raises IllegalChange
                          (fun() -> get_bst(res3)));
  "catch ends battle" >:: (fun _ -> assert_equal false 
                              (is_battle bst4));
]

let p5 = set_win pi true

let playerState_tests = [
  "init_player 1" >:: (fun _ -> assert_equal [] (owned_creatures pi));
  "init_player 2" >:: (fun _ -> assert_equal [] (get_items pi));
  "init_player 3" >:: (fun _ -> assert_equal None (get_curr pi));
  "init_player 4" >:: (fun _ -> assert_equal false (get_win pi));
  "test add item" >:: (fun _ -> assert_equal ["potion"] (get_items p2));
  "test remove item" >::(fun _ -> assert_equal []
                            (rem_item p2 "potion" (get_items p2)));
  "test set_curr" >:: (fun _ -> assert_equal cr_sq (get_cr (get_curr p1)));
  "test set_win" >:: (fun _ -> assert_equal true (get_win p5));
  "test add_creature" >:: (fun _ -> assert_equal [cr_sq] 
                              (owned_creatures (add_creature pi cr_sq)));
  "test rem_cr" >:: (fun _ -> assert_equal [] (rem_cr p_nn "John Squirtel" 
                                                 (owned_creatures p_nn)));
  "test items" >:: (fun _ -> assert_equal "" (items pi));
  "test items 2" >:: (fun _ -> assert_equal "potion" (items p2));
  "test items 3" >:: (fun _ -> assert_equal "catchball and potion" (items p3));
  "test creatures" >:: (fun _ -> assert_equal "" (creatures pi));
  "test creatures 2" >:: (fun _ -> assert_equal
                             "(John Squirtel, Squirtel, Lvl. 1)" 
                             (creatures p_nn));
]

let water_gun = List.hd (creature_attacks cr_sq)

let creature_tests = [
  "test squirtel name" >:: (fun _ -> assert_equal "Squirtel"
                               (creature_name cr_sq));
  "test squirtel type" >:: (fun _ -> assert_equal Water
                               (creature_type cr_sq));
  "test squirtel level" >:: (fun _ -> assert_equal 1
                                (creature_level cr_sq));
  "test squirtel exp" >:: (fun _ -> assert_equal 0
                              (creature_exp cr_sq));
  "test squirtel exp to level up" >:: (fun _ -> assert_equal 50
                                          (creature_exp_lvl_up cr_sq));
  "test squirtel max hp" >:: (fun _ -> assert_equal 30
                                 (creature_max_hp cr_sq));
  "test squirtel nickname" >:: (fun _ -> assert_equal "John Squirtel"
                                   (creature_nickname squirtel_named));
  "test squirtel attack name" >:: (fun _ -> assert_equal "Water Gun"
                                      (attack_name water_gun));
  "test squirtel attack type" >:: (fun _ -> assert_equal Water
                                      (attack_type water_gun));
  "test squirtel attack power" >:: (fun _ -> assert_equal 10
                                       (attack_power water_gun));
  "test squirtel level up" >:: (fun _ -> assert_equal 2
                                   (level_up cr_sq 60 1));
  "test squirtel remaining exp" >:: (fun _ -> assert_equal 10
                                        (rem_exp cr_sq 60));
  "test tarmander damage from water gun" >:: (fun _ -> assert_equal 20
                                                 (damage tarmander water_gun));
  "test valid attack" >:: (fun _ -> assert_equal true
                              (valid_attack "Tsunami" cr_sq)); 
  "test invalid attack" >:: (fun _ -> assert_equal false
                                (valid_attack "Leaf Throw" cr_sq));                                            
]

let suite =
  "test suite for project"  >::: List.flatten [
    map_tests;
    mapCommand_tests;
    mapState_tests;
    battleCommand_tests;
    battleState_tests;
    playerState_tests;
    creature_tests;
  ]

let _ = run_test_tt_main suite