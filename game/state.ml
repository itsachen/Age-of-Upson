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
			| Building(bid) -> 
				let (_,_,_,tile) = getBuildingStatus s bid in
				let dis = Util.distance apos (position_of_tile tile) in
				dis <= length_of_range range
      | Unit(uid) -> 
				let (_,_,_,tpos) = getUnitStatus s uid in
				let dis = Util.distance apos tpos in
				dis <= length_of_range range

		
	
	
			
	
