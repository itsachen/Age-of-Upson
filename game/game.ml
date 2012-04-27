open Definitions
open Constants
open Util
open State

type game = state * Mutex.t
               
let initGame () : game = 
	let s = createState ()
	and m = Mutex.create ()
  and woods = List.map (fun x -> (x,Wood,cINITIAL_WOOD)) cWOOD_TILES
  and foods = List.map (fun x -> (x,Food,cINITIAL_FOOD)) cFOOD_TILES in
	setResources s (woods@foods);
  Netgraphics.send_update (InitGraphics);
	Netgraphics.send_update (InitFood cFOOD_TILES);
	Netgraphics.send_update (InitWood cWOOD_TILES);
  (s, m)

let initUnitsAndBuildings g : unit =
	let (s,m) = g 
	and (rx,ry) = (100.,100.)
	and (bx,by) = (400.,400.) in
	setTeamBuildings s Red [(next_available_id (), TownCenter,
		cTOWNCENTER_HEALTH, tile_of_pos (rx,ry) ) ];
	let (_,_,rb,_,_,_,_) = getTeamStatus s Red in
  List.iter
		(fun (id,t,h,p) ->
			Netgraphics.add_update (AddBuilding (id,t,p,h,Red));() ) rb; 
	setTeamBuildings s Blue [(next_available_id (), TownCenter, 
		cTOWNCENTER_HEALTH, tile_of_pos (bx,by)) ];
	let (_,_,bb,_,_,_,_) = getTeamStatus s Blue in
  List.iter
		(fun (id,t,h,p) ->
			Netgraphics.add_update (AddBuilding (id,t,p,h,Blue));() ) bb;	 
	for i = 1 to cSTARTING_VILLAGER_COUNT do
		addTeamUnit s Red (next_available_id (), Villager, 
			cVILLAGER_HEALTH, (rx+.5.*.float_of_int i,ry));
		addTeamUnit s Blue (next_available_id (), Villager, 
			cVILLAGER_HEALTH, (bx+.5.*.float_of_int i,by));
	done ;
	let (_,ru,_,_,_,_,_) = getTeamStatus s Red in
  List.iter
		(fun (id,t,h,p) ->
			Netgraphics.add_update (AddUnit (id,t,p,h,Red));() ) ru;
	let (_,bu,_,_,_,_,_) = getTeamStatus s Blue in
  List.iter
		(fun (id,t,h,p) ->
			Netgraphics.add_update (AddUnit (id,t,p,h,Blue));() ) bu
	

let startGame g : unit = 
	let (s,m) = g in
	setTimer s 0. ; 
	()

let handleAction g act c : command = 
  let (s,m) = g in 
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
  let (s,m) = g in 
  Mutex.lock m;
  let res = check_for_game_over s new_time in
  (match res with
   | Some c -> ()
   | None -> 
       failwith "not implemented");
  Mutex.unlock m;
  res
