open Map
open BattleCommand
open MapState
open PlayerState
open Creature

type t = {
  battle : bool;
  enemy_creature : Creature.t option;
  current_creature : Creature.t option;
  enemy_hp : int;
  curr_hp : int;
}

type result = 
  |Legal of t
  |Illegal

exception NoAttacks

exception IllegalChange

let is_battle st = 
  st.battle

(** [get_em_opt st] is the enemy creature option in battle state [st]. *)
let get_em_opt st =
  st.enemy_creature

(** [get_curr_opt st] is the current creature option in battle state [st]. *)
let get_curr_opt st =
  st.current_creature

let get_enemy st =
  get_cr st.enemy_creature

let get_curr_cr st =
  get_cr st.current_creature

let get_curr_hp st = st.curr_hp

let get_enemy_hp st = st.enemy_hp

let init_battle_state = {
  battle = false;
  enemy_creature = None;
  current_creature = None;
  enemy_hp = 0;
  curr_hp = 0;
}

let set_battle st v = {
  battle = v;
  enemy_creature = st.enemy_creature;
  current_creature = st.current_creature;
  enemy_hp = st.enemy_hp;
  curr_hp = st.curr_hp;
}

let set_em_hp st v = {
  battle = st.battle;
  enemy_creature = st.enemy_creature;
  current_creature = st.current_creature;
  enemy_hp = v;
  curr_hp = st.curr_hp;
}

let set_curr_hp st v = {
  battle = st.battle;
  enemy_creature = st.enemy_creature;
  current_creature = st.current_creature;
  enemy_hp = st.enemy_hp;
  curr_hp = v;
}

let set_battle_state b em cr = {
  battle = b;
  enemy_creature = em;
  current_creature = cr;
  enemy_hp = creature_max_hp (get_cr em);
  curr_hp = creature_max_hp (get_cr cr);
}

let rem_em_hp st at cr = 
  let cl = creature_attacks cr in
  let rec change_helper at acc =
    match acc with
    |h::t -> if (String.compare at (attack_name h) = 0) then h
      else change_helper at t
    |[] -> raise NoAttacks
  in
  if (st.enemy_hp) <= (damage (get_enemy st) (change_helper at cl))
  then 0
  else (st.enemy_hp) - (damage (get_enemy st) (change_helper at cl))

let rec get_attack r i lst = 
  match lst with
  |h::t -> if r = i then h else get_attack r (i+1) t
  |[] -> raise NoAttacks

let rem_curr_hp st r lst=
  if (st.curr_hp) <= (damage (get_curr_cr st) (get_attack r 0 lst))
  then 0
  else (st.curr_hp) - (damage (get_curr_cr st) (get_attack r 0 lst))

let add_curr_hp cr st it = 
  match it with
  |"potion" -> (let hp = st.curr_hp in
                if (hp + 10) >= creature_max_hp cr then creature_max_hp cr
                else hp + 10)
  |_ -> 0

let attack st cr at = 
  if (valid_attack at cr) && (st.curr_hp > 0) && (st.enemy_hp > 0) 
  then Legal (st |> get_curr_cr |> rem_em_hp st at |> set_em_hp st)
  else Illegal

let catch st (em_cr : Creature.t) player =
  if ((st.enemy_hp) <= ((creature_max_hp em_cr)/2)) && 
     (st.enemy_hp) <> 0 && List.mem "catchball" (get_items player)
  then Legal {
      battle = false;
      enemy_creature = st.enemy_creature;
      current_creature = st.current_creature;
      enemy_hp = st.enemy_hp;
      curr_hp = st.curr_hp;
    }
  else Illegal

let flee st = 
  let r = Random.int (10) in
  if r < 8 then
    Legal {
      battle = false;
      enemy_creature = st.enemy_creature;
      current_creature = st.current_creature;
      enemy_hp = st.enemy_hp;
      curr_hp = st.curr_hp;
    }
  else Illegal

let use st p it = 
  if (String.equal it "potion") && (List.mem it (get_items p)) && 
     (st.enemy_hp <> 0) then
    Legal {
      battle = true;
      enemy_creature = st.enemy_creature;
      current_creature = st.current_creature;
      enemy_hp = st.enemy_hp;
      curr_hp = add_curr_hp (get_curr_cr st) st it;
    }
  else Illegal

let get_bst result =
  match result with
  | Legal t -> t
  | Illegal -> raise IllegalChange
