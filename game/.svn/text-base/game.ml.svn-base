open Definitions
open Constants
open Util

type game = int ref * Mutex.t (*No, This needs to be changed based on your implementation*)

let initGame () : game = failwith "not implemented"

let initUnitsAndBuildings (s, m) : unit = failwith "not implemented"

let startGame g : unit = failwith "not implemented"

let handleAction g act c : command = 
  let (s, m) = g in
  Mutex.lock m;
  let res =
    (* will involve having to get this unit_id's team color,
     * and checking it against c. Return Failed if the two
     * colors are not equal. Else, match against all the possible actions.
     *)
    match act with
		| QueueCollect unit_id -> failwith "not implemented"
		| QueueMove(unit_id,pos) -> failwith "not implemented"
    | Talk str -> Netgraphics.add_update(DisplayString(c, str)); Success
		| QueueAttack (unit_id, attackable_object) -> failwith "not implemented"
		| QueueBuild (unit_id, building_type) -> failwith "not implemented"
		| QueueSpawn (building_id, unit_type) -> failwith "not implemented"
		| ClearAttack id -> failwith "not implemented" 
		| ClearMove id -> failwith "not implemented"
		| Upgrade upgrade_type -> failwith "not implemented"
		in
  Mutex.unlock m;
  Result res

let handleStatus g status : command = 
  let (s, m) = g in
  Mutex.lock m;
  let data =
    match status with
			| TeamStatus c -> failwith "not implemented"
			| UnitStatus id -> failwith "not implemented"
			| BuildingStatus id -> failwith "not implemented"
			| GameStatus -> failwith "not implemented"
			| ResourceStatus -> failwith "not implemented"
    in
  Mutex.unlock m;
  Data data

let check_for_game_over s curr_time : game_result option =
	failwith "not implemented"

let handleTime g new_time : game_result option = 
  let (s, m) = g in
  Mutex.lock m;
  let res = check_for_game_over !s new_time in
  (match res with
   | Some c -> ()
   | None -> 
       failwith "not implemented");
  Mutex.unlock m;
  res
