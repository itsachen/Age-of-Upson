open Definitions
open Constants
open Util
open State

type game = state * Mutex.t
               
let initGame () : game = 
   let woods = List.map (fun x -> (x,Wood,cINITIAL_WOOD)) cWOOD_TILES
   and foods = List.map (fun x -> (x,Food,cINITIAL_FOOD)) cFOOD_TILES
   and t = ref (int_of_float cTIME_LIMIT)
   and red = {color= Red;score=0;units=ref [];buildings= ref [];
      age=DarkAge;food=0;wood=0;upgrades=(false,false,false)}
   and blue = {color= Blue;score=0;units=ref [];buildings=ref [];
      age=DarkAge;food=0;wood=0;upgrades=(false,false,false)} in
   Netgraphics.send_update (InitGraphics);
   {team_red = red; team_blue = blue; resources = woods@foods;
      timer = t; m = Mutex.create()}

let initUnitsAndBuildings g : unit =
	Netgraphics.send_update (InitFood cFOOD_TILES);
	Netgraphics.send_update (InitWood cWOOD_TILES);
	g.team_red.units := [];
	g.team_blue.units := [];

	()

let startGame g : unit = 
	()

let handleAction g act c : command = 
<<<<<<< HEAD
  let (s,m)= g in 
=======
  let (s,m) = g in 
>>>>>>> nothing
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
  let (s,m) = g in
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
  let m = g.m 
	and s = g.timer in 
  Mutex.lock m;
  let res = check_for_game_over !s new_time in
  (match res with
   | Some c -> ()
   | None -> 
       failwith "not implemented");
  Mutex.unlock m;
  res
