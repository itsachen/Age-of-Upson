open Definitions
open Constants
open Util
open Hashqueue

type team = {color: color; score:score ref; units:unit_data list ref; 
   buildings:building_data list ref; age:age ref; food:food_count ref; 
	 wood:wood_count ref; upgrades:upgrades ref}

type state= {team_red: team; team_blue:team; 
   resources: resource_data list ref; timer: float ref; 
   movq: hashqueue ref; gatherq: hashqueue ref; attackq: hashqueue ref; 
   buildq: hashqueue ref; spawnq: hashqueue ref; cdtable: (unit_id,timer) Hashtbl.t ref}
	
let createState (u: unit): state = 
	let red = {color= Red;score=ref 0;units=ref [];buildings=ref [];
      age=ref DarkAge;food=ref 0;wood=ref 0;upgrades=ref (false,false,false)}
	and blue = {color= Blue;score=ref 0;units=ref [];buildings= ref [];
      age=ref DarkAge;food=ref 0;wood=ref 0;upgrades=ref (false,false,false)} 
	and mq = createHashq 0 
	and gq = createHashq 0 
	and aq = createHashq 0 
	and bq = createHashq 0 
	and sq = createHashq 0  
	and cd = Hashtbl.create 0 in
	{team_red= red; team_blue= blue; 
   resources= ref []; timer= ref 0.; 
   movq=ref mq; gatherq=ref gq; attackq=ref aq; 
   buildq=ref bq; spawnq= ref sq; cdtable = ref cd} 

let getTeamStatus (s: state) (c: color): team_data=
   match c with
   | Red -> 
      (let r= s.team_red in 
         (!(r.score), !(r.units), !(r.buildings), !(r.age), 
					!(r.food), !(r.wood), !(r.upgrades) ))
   | Blue ->
      (let b= s.team_blue in 
         (!(b.score), !(b.units), !(b.buildings), !(b.age), 
					!(b.food), !(b.wood), !(b.upgrades) ))
  

(* @# WHAT HAPPENS IF UNIT_ID OR BUILDING_ID IS NOT IS NOT THERE *)
let getUnitStatus (s: state) (i: unit_id): unit_data=
   let foo= List.fold_left
      (fun a c ->
         match c with
         | (id,ty,h,p) -> if id = i then c else a) 
      (-1,Villager,-1,(-1.0,-1.0)) !(s.team_red.units) in
   if foo = (-1,Villager,-1,(-1.0,-1.0)) then
      (List.fold_left 
         (fun a c ->
            match c with
            | (id,ty,h,p) -> if id = i then c else a) 
      (-1,Villager,-1,(-1.0,-1.0)) !(s.team_blue.units) )
   else foo

let getBuildingStatus (s: state) (i: building_id): building_data=
   let foo= List.fold_left
      (fun a c ->
         match c with
         | (id,ty,h,ti) -> if id = i then c else a) 
      (-1,Barracks,-1,(-1,-1)) !(s.team_red.buildings) in
   if foo = (-1,Barracks,-1,(-1,-1)) then
      (List.fold_left 
         (fun a c -> 
            match c with
            | (id,ty,h,ti) -> if id = i then c else a) 
      (-1,Barracks,-1,(-1,-1)) !(s.team_blue.buildings) )
   else foo

let getGameStatus (s: state) : game_data=
   (getTeamStatus s Red,getTeamStatus s Blue, !(s.timer))

(* Double check? *)
let getResourceStatus (s: state) : resource_data list=
   !(s.resources)
	
let getTimer (s:state) : timer =
	!(s.timer)
	
let getCDTable (s:state) : (unit_id,timer) Hashtbl.t =
	!(s.cdtable)
	
	
let setTeamScore (s:state) (c:color) (sc: score):unit =
	match c with
		| Red -> s.team_red.score := sc; ()
		| Blue -> s.team_blue.score := sc; ()

let addTeamScore (s:state) (c:color) (sc: score):unit =
	match c with
		| Red -> s.team_red.score := (!(s.team_red.score) + sc); ()
		| Blue -> s.team_blue.score := (!(s.team_red.score) + sc); ()

let addTeamUnit (s:state) (c:color) (u: unit_data):unit =
	match c with
		| Red -> s.team_red.units := u:: !(s.team_red.units); ()
		| Blue -> s.team_blue.units := u:: !(s.team_blue.units); ()

let removeTeamUnit (s:state) (c:color) (id: unit_id):unit =
	let rec aux acc = function
  | [] -> List.rev acc
  | (i,_,_,_) as x::xs ->
      if id = i
      then aux acc xs
      else aux (x::acc) xs
  in
	match c with
		| Red -> s.team_red.units := aux [] !(s.team_red.units); ()
		| Blue -> s.team_blue.units := aux [] !(s.team_blue.units); ()

let updateTeamUnit (s:state) (c:color) (u: unit_data):unit =
	let (id,_,_,_) = u in
	removeTeamUnit s c id;
	addTeamUnit s c u
	
let addTeamBuilding (s:state) (c:color) (b: building_data):unit =
	match c with
		| Red -> s.team_red.buildings := b:: !(s.team_red.buildings); ()
		| Blue -> s.team_blue.buildings := b:: !(s.team_blue.buildings); ()

let removeTeamBuilding (s:state) (c:color) (id: building_id):unit =
	let rec aux acc = function
  | [] -> List.rev acc
  | (i,_,_,_) as x::xs ->
      if id = i
      then aux acc xs
      else aux (x::acc) xs
  in
	match c with
		| Red -> s.team_red.buildings := aux [] !(s.team_red.buildings); ()
		| Blue -> s.team_blue.buildings := aux [] !(s.team_blue.buildings); ()
	
let updateTeamBuilding (s:state) (c:color) (b: building_data):unit =
	let (id,_,_,_) = b in
	removeTeamBuilding s c id;
	addTeamBuilding s c b
	
let setTeamBuildings (s:state) (c:color) (b: building_data list):unit =
	match c with
		| Red -> s.team_red.buildings := b; ()
		| Blue -> s.team_blue.buildings := b; ()
		
let setTeamAge (s:state) (c:color) (a: age):unit =
	match c with
		| Red -> s.team_red.age := a; ()
		| Blue -> s.team_blue.age := a; ()

let setTeamFood (s:state) (c:color) (f: food_count):unit =
	match c with
		| Red -> s.team_red.food := f; ()
		| Blue -> s.team_blue.food := f; ()

let setTeamWood (s:state) (c:color) (w: wood_count):unit =
	match c with
		| Red -> s.team_red.wood := w; ()
		| Blue -> s.team_blue.wood := w; ()

let setTeamUpgrades (s:state) (c:color) (u: upgrades):unit =
	match c with
		| Red -> s.team_red.upgrades := u; ()
		| Blue -> s.team_blue.upgrades := u; ()

let setResources (s:state) (r: resource_data list):unit =
	s.resources := r; ()
	
let setTimer (s:state) (t: float):unit =
	s.timer := t; ()
	
let setMovq (s:state) (q: hashqueue):unit =
	s.movq := q; ()
	
let setGatherq (s:state) (q: hashqueue):unit =
	s.gatherq := q; ()

let setAttackq (s:state) (q: hashqueue):unit =
	s.attackq := q; ()
	
let setBuildq (s:state) (q: hashqueue):unit =
	s.buildq := q; ()
	
let setSpawnq (s:state) (q: hashqueue):unit =
	s.spawnq := q; ()
	
let updateCDTable (s:state) ((id:unit_id),(t:timer)):unit =
	let cd = getCDTable s in
	Hashtbl.replace cd id t

(* CHANGE BOTH TO LIST.FIND_ALL *)
(* Returns the team that the UID is. *)
let getTeam uid s: color option=
   let foo= List.fold_left
      (fun a c ->
         match c with
         | (id,ty,h,p) -> if id = uid then c else a) 
      (-1,Villager,-1,(-1.0,-1.0)) !(s.team_red.units) in
   if foo = (-1,Villager,-1,(-1.0,-1.0)) then
      (let foo2= List.fold_left 
         (fun a c ->
            match c with
            | (id,ty,h,p) -> if id = uid then c else a) 
      (-1,Villager,-1,(-1.0,-1.0)) !(s.team_blue.units) in
      if foo2= (-1,Villager,-1,(-1.0,-1.0)) then None
      else Some(Blue))
   else Some(Red) 
	
let getUnitColor (s:state) (uid:unit_id): color option =
	if (List.filter (fun (x,_,_,_) -> x=uid) 
		!(s.team_red.units)) <> [] then Some Red
	else if (List.filter (fun (x,_,_,_) -> x=uid) 
		!(s.team_blue.units)) <> [] then Some Blue
	else None
		
let getBuildingColor (s:state) (bid:building_id): color option =
	if (List.filter (fun (x,_,_,_) -> x=bid) 
		!(s.team_red.buildings)) <> [] then Some Red
	else if (List.filter (fun (x,_,_,_) -> x=bid) 
		!(s.team_blue.buildings)) <> [] then Some Blue
	else None
		
let getType uid s: unit_type option=
   let foo= List.fold_left
      (fun a c ->
         match c with
         | (id,ty,h,p) -> if id = uid then c else a) 
      (-1,Villager,-1,(-1.0,-1.0)) !(s.team_red.units) in
   if foo = (-1,Villager,-1,(-1.0,-1.0)) then
      (let foo2= List.fold_left 
         (fun a c ->
            match c with
            | (id,ty,h,p) -> if id = uid then c else a) 
      (-1,Villager,-1,(-1.0,-1.0)) !(s.team_blue.units) in
      if foo2= (-1,Villager,-1,(-1.0,-1.0)) then None
      else let (_,ty,_,_)= foo2 in Some(ty))
   else let (_,ty,_,_)= foo in Some(ty)

let getIsBuilding uid s: bool=
   let foo= List.filter 
      (fun (id,ty,h,til) -> uid = id) !(s.team_red.buildings) in
   match foo with
   | [] -> 
      (let foo2= List.filter
      (fun (id,ty,h,til) -> uid = id) !(s.team_blue.buildings) in
      if foo2 = [] then false else true)
   | _ -> true
	
let validAttack (s:state) (currTime: timer) ((a:unit_id),(t:attackable_object)) : bool =
	let cdtable = getCDTable s in
	let attTime = Hashtbl.find cdtable a in
	if attTime > currTime then false
	else
		let att_color = getUnitColor s a in
		let (aid,aty,_,apos) = getUnitStatus s a in
		let range = 
			match aty with
				| Archer ->  cARCHER_RANGE
				| EliteArcher -> cELITE_ARCHER_RANGE
				| Pikeman -> cPIKEMAN_RANGE
				| ElitePikeman -> cELITE_PIKEMAN_RANGE
				| Knight -> cKNIGHT_RANGE
				| EliteKnight -> cELITE_KNIGHT_RANGE
				| _ -> 0.
		in
		match t with
			| Building(bid) -> (
				match (getBuildingColor s bid) with
					| None -> false
					| c -> 
						if c = att_color then false
						else (
							let (_,_,_,tile) = getBuildingStatus s bid in
							let dis = Util.distance apos (position_of_tile tile) in
							dis <= length_of_range range))
      | Unit(uid) -> (
				match (getUnitColor s uid) with
					| None -> false
					| c -> 
						if c = att_color then false 
						else (
							let (_,_,_,tpos) = getUnitStatus s uid in
							let dis = Util.distance apos tpos in
							dis <= length_of_range range) )

let isAdvantage att_type tar_type : bool =
	match	(att_type,tar_type) with
		| (Pikeman,Knight) -> true
		| (Pikeman,EliteKnight) -> true
		| (ElitePikeman,Knight) -> true
		| (ElitePikeman,EliteKnight) -> true
		| (Archer,Pikeman) -> true
		| (EliteArcher,Pikeman) -> true
		| (Archer,ElitePikeman) -> true
		| (EliteArcher,ElitePikeman) -> true
		| (Knight,Archer) -> true
		| (Knight,EliteArcher) -> true
		| (EliteKnight,Archer) -> true
		| (EliteKnight,EliteArcher) -> true
		| _ -> false
	
let getTeamAge (s:state) (c:color) : age =
	match c with
		| Red -> !(s.team_red.age)
		| Blue -> !(s.team_blue.age)

let updateResource (s:state) (r:resource_data):unit =
	let (tile,_,_) = r in
	let new_resources = List.fold_left (fun a x ->
		let (r_tile,_,_) = x in
		if r_tile = tile then r::a
		else x::a ) [] (getResourceStatus s) in
	setResources s new_resources
			
let getTeamScore (s:state) (c:color): score =
	match c with
		| Red -> !(s.team_red.score)
		| Blue -> !(s.team_blue.score)


let queueCollect (s:state) uid c copt tyopt: result=
   (* Check if uid is of same color*)
   match copt with
   | Some(cresult) -> 
      (if c = cresult then 
         (match tyopt with
          | Some(Villager) -> (
            let (u_id,u_type,u_h,u_pos) = getUnitStatus s uid in
						let u_tile = tile_of_pos u_pos in
						let resources = getResourceStatus s in
						let r = List.filter (fun (r_tile,_,_) -> 
							r_tile = u_tile) resources in
						match r with
							| [] -> Failed
							| _ -> (
								let (r_tile,r_ty,count)= List.hd r in
								let inc = 
									match (getTeamAge s c) with
										| DarkAge -> cRESOURCE_COLLECTED
										| ImperialAge -> cADVANCED_RESOURCE_COLLECTED
								in let r_inc = 
									if inc <= count then inc
									else count
								in updateResource s (r_tile,r_ty,count-r_inc);
								Netgraphics.add_update (DoCollect (u_id,c,r_ty,r_inc));
								addTeamScore s c r_inc;
								Netgraphics.add_update (UpdateScore (c,getTeamScore s c));
								Success	) )
          | _ -> Failed (* uid does not exist *) )
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)

let queueMove (s:state) (c:color) (copt: color option) (uid:unit_id) (dest:vector):result=
	match copt with
		| None -> Failed
		| Some color -> 
			if color <> c then Failed
			else (
				if not (is_valid_pos dest ) then Failed
				else
					let movequeue = !(s.movq) in
					let q = Hashtbl.find movequeue uid in
					match q with 
						| MoveQueue(mq) -> (
							let (_,u_ty,_,u_pos) = getUnitStatus s uid in
							let (x1,y1) = u_pos
							and (x2,y2) = dest
							and speed = get_unit_type_speed u_ty in
							let d = distance (x1,y1) (x2,y2) in
							let step = (speed*.(x2-.x1)/.d,speed*.(y2-.y1)/.d) in
							let rec helpEnq (xc,yc) (xd,yd) (x0,y0): result =
								if distance (xc,yc) (xd,yd) < speed
								then (Queue.push (uid,(xd,yd)) mq; Success)
								else (Queue.push (uid,(xc+.x0,yc+.y0)) mq; 
											helpEnq (xc+.x0,yc+.y0) (xd,yd) (x0,y0)) in
							helpEnq (x1,y1) (x2,y2) step ) 
						| _ -> Failed )
		
