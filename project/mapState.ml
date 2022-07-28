open Map
open MapCommand
open Creature
open PlayerState

exception IllegalMove

type t = {
  position : int * int
}

type result = Legal of t | Illegal

let init_state m =
  {
    position = initial_position m
  }

let position st =
  st.position

(**[new_position_north st] returns a new position if the player in state [st]
   moves north once.*)
let new_position_north st =
  match position st with
  | (x,y) -> (x, y-1)

(**[new_position_north st] returns a new position if the player in state [st]
   moves south once.*)
let new_position_south st =
  match position st with
  | (x,y) -> (x, y+1)

(**[new_position_north st] returns a new position if the player in state [st]
   moves west once.*)
let new_position_west st =
  match position st with
  | (x,y) -> (x-1, y)

(**[new_position_north st] returns a new position if the player in state [st]
   moves east once.*)
let new_position_east st =
  match position st with
  | (x,y) -> (x+1, y)

(**[is_walkable_tile m pos] returns true if the tile at [pos] in map [m] can
   be walked on, assuming [pos] is in the map [m].
   Otherwise, it returns false.*)
let is_walkable_tile m pos =
  match tile_at_position m pos with
  | Path -> true
  | Grass -> true
  | Tree -> false

(**[is_walkable m pos] returns true if position [pos] can be
   walked on in map [m]. Otherwise, it returns false*)
let is_walkable m pos =
  match pos with
  |(x,y) ->
    if legal_position m pos
    then is_walkable_tile m pos
    else false

let match_direction dir st =
  match dir with 
  | North -> st |> new_position_north
  | South -> st |> new_position_south 
  | East -> st |> new_position_east
  | West -> st |> new_position_west 

let set_position pos = {
  position = pos
}

let move dir m st p =
  let check_movable_space dir m st =
    is_walkable m (match_direction dir st)
  in
  if (check_movable_space dir m st) && (List.length (owned_creatures p) <> 0)
  then Legal {position = match_direction dir st}
  else Illegal

let get_m_state result =
  match result with
  | Legal t -> t
  | Illegal -> raise IllegalMove

(**[valid_pick cr] is a helper function that returns true if creature [cr] is a 
    valid starter creature *)
let valid_pick cr = 
  match cr with
  |"Squirtel" -> true
  |"Bulbasoar" -> true
  |"Tarmander" -> true
  |_ -> false

let pick st cr = 
  if (valid_pick cr) then 
    Legal {
      position = st.position
    }
  else Illegal

let rec get_set cr lst p =
  match lst with
  |h::t -> if creature_nickname h = cr then Some h
    else get_set cr t p
  |[] -> None

(** [valid_set cr lst p] is a function that returns true if creature [cr] is
    a valid creature out of the list of creatures [lst] for the player [p] to 
    set as their current creature. *)
let rec valid_set cr lst p = 
  match lst with
  |h::t -> if creature_nickname h = cr then true
    else valid_set cr t p
  |[] -> false

let set st cr p =
  if (valid_set cr (owned_creatures p) p) then
    Legal {
      position = st.position
    }
  else Illegal