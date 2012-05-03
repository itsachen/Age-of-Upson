open Definitions
open Constants
open Util
open State
open Hashqueue

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

(*generate random and valid initial positions *)	
let rec randPos (c:color): float*float =
	Random.self_init ();
	match c with
		| Red -> let x = Random.float (cBOARD_WIDTH/.2. -. cTILE_WIDTH*.2.)
		         and y = Random.float (cBOARD_HEIGHT -. cTILE_HEIGHT*.2.) in
						 if (is_valid_pos (x,y)) && (is_valid_tile (tile_of_pos (x,y))) then (x,y)
						 else randPos c
		| Blue -> let x = (Random.float (cBOARD_WIDTH/.2. -. cTILE_WIDTH*.2.)) +. cBOARD_WIDTH/.2.
		         and y = Random.float (cBOARD_HEIGHT -. cTILE_HEIGHT*.2.) in
						 if (is_valid_pos (x,y)) && (is_valid_tile (tile_of_pos (x,y))) then (x,y)
						 else randPos c

let initUnitsAndBuildings g : unit =
	let (s,m) = g 
	and (rx,ry) = randPos Red
	and (bx,by) = randPos Blue in
	Mutex.lock m;
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
			Netgraphics.add_update (AddUnit (id,t,p,h,Red));
			updateCDTable s (id,0.); () ) ru;
	let (_,bu,_,_,_,_,_) = getTeamStatus s Blue in
  List.iter
		(fun (id,t,h,p) ->
			Netgraphics.add_update (AddUnit (id,t,p,h,Blue));
			updateCDTable s (id,0.); () ) bu;
	Mutex.unlock m
	

let startGame g : unit = 
	let (s,m) = g in
	Mutex.lock m;
	setTimer s (Unix.gettimeofday ()) ; 
	Mutex.unlock m


let handleAction g act c : command = 
  let (s,m) = g in 
  Mutex.lock m;
  let res = 
    (* will involve having to get this unit_id's team color,
     * and checking it against c. Return Failed if the two
     * colors are not equal. Else, match against all the possible actions.
     *)
    match act with
    | QueueCollect unit_id -> 
			State.queueCollect s unit_id c (getUnitColor s unit_id) (getType unit_id s)		

    | QueueMove(unit_id,pos) -> 
       State.queueMove s c (getUnitColor s unit_id) unit_id pos

    | Talk str -> Netgraphics.add_update(DisplayString(c, str)); Success

    | QueueAttack (unit_id, attackable_object) ->
     queueAttack (unit_id, attackable_object) !(s.attackq) c (getTeam unit_id s)
          (getType unit_id s)

    | QueueBuild (unit_id, building_type) ->
       queueBuild (unit_id, building_type) !(s.buildq) c (getTeam unit_id s)
          (getType unit_id s)

    | QueueSpawn (building_id, unit_type) ->
       queueSpawn (building_id, unit_type) !(s.spawnq) c 
       (getTeam building_id s) (getIsBuilding building_id s)

    | ClearAttack id ->
       clearAttack id !(s.attackq) c (getTeam id s)
          (getType id s)

    | ClearMove id ->
       clearMove id !(s.movq) c (getTeam id s)

    | Upgrade upgrade_type -> failwith "not implemented"
		in
  Mutex.unlock m;
  Result res

let handleStatus g status : command = 
  let (s,m) = g in
  Mutex.lock m;
  let data =
    match status with
	| TeamStatus c -> TeamData (getTeamStatus s c)
	| UnitStatus id -> UnitData (getUnitStatus s id)
	| BuildingStatus id -> BuildingData (getBuildingStatus s id)
	| GameStatus -> GameData (getGameStatus s)
	| ResourceStatus -> ResourceData (getResourceStatus s)
    in
  Mutex.unlock m;
  Data data

let check_for_game_over s curr_time : game_result option =
	let (rs,ru,rb,_,rf,rw,_) = getTeamStatus s Red 
	and (bs,bu,bb,_,bf,bw,_) = getTeamStatus s Blue
	and timer = getTimer s in
	let redLose = (List.length ru = 0) ||
	 (List.find_all (fun (_,typ,_,_) -> typ = TownCenter) rb = [])
	and blueLose = (List.length bu = 0) ||
	 (List.find_all (fun (_,typ,_,_) -> typ = TownCenter) bb = []) in
	match (redLose,blueLose) with
		| (true,false) -> Some (Winner(Blue))
		| (false,true) -> Some (Winner(Red))
		| (true,true) -> Some Tie
		| _ -> 	
		if curr_time-. timer >= cTIME_LIMIT then
			let redScore = rf+rw
			and blueScore = bf+bw in
			if redScore > blueScore then Some (Winner(Red))
			else if blueScore > redScore then Some (Winner(Blue))
			else Some Tie
		else None


let rec helpAttack s currTime atkq =
		if Queue.is_empty atkq then ()
		else (
			let (att_id,tar) = Queue.pop atkq in 
			(*if att_id <> uid then failwith "handleAttack error";*)
			if validAttack s currTime (att_id,tar) then (
				let (_,att_ty,_,_) = getUnitStatus s att_id in
				let inc =get_unit_type_range att_ty
				and atk = get_unit_type_attack_damage att_ty
				in 
				Netgraphics.add_update (DoAttack att_id);
				updateCDTable s (att_id,currTime+.inc) ;
				match tar with
					| Building(tar_id) -> 
						let (bp1,bp2,health,bp4) = getBuildingStatus s tar_id in
						let h = 
							if health-atk < 0 then 0
							else health-atk 
						in (
							Netgraphics.add_update (UpdateBuilding (bp1, h));
							match (getBuildingColor s bp1) with 
								| Some Red -> updateTeamBuilding s Red (bp1,bp2,h,bp4)													 
								| Some Blue -> updateTeamBuilding s Blue (bp1,bp2,h,bp4)
								| None -> failwith "handleAttack error")
					| Unit(tar_id) ->
						let (up1,up2,health,up4) = getUnitStatus s tar_id in
						let atk = 
							if isAdvantage att_ty up2 then atk+cSTAB_BONUS
							else atk 
						in				
						let h = 
							if health-atk < 0 then 0
							else health-atk 
						in (
							Netgraphics.add_update (UpdateUnit (up1, h));
							match (getUnitColor s up1) with 
								| Some Red -> updateTeamUnit s Red (up1,up2,h,up4)
								| Some Blue -> updateTeamUnit s Blue (up1,up2,h,up4)
								| None -> failwith "handleAttack error")	)
				else helpAttack s currTime atkq
		)
		
						
let handleAttack s currTime: unit =
	let attackqueue = !(s.attackq) in
	Hashtbl.iter (fun uid q -> 
		match q with
			| AttackQueue(atkq) -> helpAttack s currTime atkq
			| _ -> ()
			) attackqueue
			
let removeDead (s:state) :unit =
	let red_units = !(s.team_red.units)
	and blue_units = !(s.team_blue.units)
	and red_buildings = !(s.team_red.buildings)
	and blue_buildings = !(s.team_blue.buildings) in
	let red_score1 = List.fold_left (fun a x ->
		let (id,_,health,_) = x in
		if health <= 0 then (removeTeamUnit s Red id; 
												 Netgraphics.add_update (RemoveUnit id);
												 a+cKILL_UNIT_SCORE )
		else a) 0 red_units in
	let red_score = List.fold_left (fun a x ->
		let (id,_,health,_) = x in
		if health <= 0 then (removeTeamBuilding s Red id;
												 Netgraphics.add_update (RemoveBuilding id); 
												 a+cKILL_BUILDING_SCORE )
		else a) red_score1 red_buildings in
	let blue_score1 = List.fold_left (fun a x ->
		let (id,_,health,_) = x in
		if health <= 0 then (removeTeamUnit s Blue id;
												 Netgraphics.add_update (RemoveUnit id); 
												 a+cKILL_UNIT_SCORE )
		else a) 0 blue_units in
	let blue_score = List.fold_left (fun a x ->
		let (id,_,health,_) = x in
		if health <= 0 then (removeTeamBuilding s Blue id; 
												 Netgraphics.add_update (RemoveBuilding id);
												 a+cKILL_BUILDING_SCORE )
		else a) blue_score1 blue_buildings in
		setTeamScore s Red (!(s.team_red.score)+red_score);
		setTeamScore s Blue (!(s.team_blue.score)+blue_score);
		Netgraphics.add_update (UpdateScore (Red,!(s.team_red.score)) );
		Netgraphics.add_update (UpdateScore (Blue,!(s.team_blue.score)) )
	
let removeResource (s:state) : unit =
	let resources = !(s.resources) in
	let new_resources = List.fold_left (fun a x -> 
		let (tile,_,count) = x in
		if count <=0 then (Netgraphics.add_update (RemoveResource tile); a)
		else x::a
		) [] resources in
	setResources s new_resources 
	
let handleBuildingCreation s currTime :unit =
	let buildqueue = !(s.buildq) in
	Hashtbl.iter (fun uid q -> 
		match q with
			| BuildQueue(bq) -> 
				(if Queue.is_empty bq then ()
				 else 
					let (uid,_) = Queue.peek bq in
					let (_,_,_,u_pos) = getUnitStatus s uid in
					let cd = Hashtbl.find (getCDTable s) uid in
					if cd <= currTime then let (u,b_ty)=Queue.pop bq in
					let color = getUnitColor s u in
					match color with 
						| Some c -> (
							let b_id = next_available_id ()
							and b_h = get_building_type_health b_ty
							and b_tile = tile_of_pos u_pos in
							addTeamBuilding s c 
							(b_id, b_ty,b_h,b_tile);
							Netgraphics.add_update (AddBuilding (b_id,b_ty,b_tile,b_h,c))
							)
						| None -> () )
			| _ -> ()
			) buildqueue
	
let moveUnits (s:state) currTime: unit =
	let movequeue = !(s.movq) in
	Hashtbl.iter (fun uid q -> 
		match q with
			| MoveQueue(mq) -> (
				if Queue.is_empty mq then ()
				else 
					let (id,v) = Queue.pop mq in
					let (u_id,u_ty,u_h,_) = getUnitStatus s id in
					if u_ty = Villager && (Hashtbl.find (getCDTable s) u_id) > currTime
					then ();
					match (getUnitColor s u_id) with
						| None -> ()
						| Some c -> (
							updateTeamUnit s c (u_id,u_ty,u_h,v);
							Netgraphics.add_update (MoveUnit (u_id,[v],c))
							)
				)
			| _ -> ()) movequeue


let handleTime g new_time : game_result option = 
  let (s,m) = g in 
  Mutex.lock m;
  let res = check_for_game_over s new_time in
  (match res with
   | Some c -> ()
   | None -> 
		(handleAttack s new_time;
		removeDead s;
		removeResource s;
		handleBuildingCreation s new_time;
		moveUnits s new_time; ) );
  Mutex.unlock m;
  res
