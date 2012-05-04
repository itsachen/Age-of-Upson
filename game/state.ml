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
	and mq = createHashq 100 
	and gq = createHashq 100 
	and aq = createHashq 100 
	and bq = createHashq 100 
	and sq = createHashq 100  
	and cd = Hashtbl.create 100 in
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
	
let getUnitCosts u_ty : food_count*wood_count =
	match u_ty with
		| Villager -> cSPAWN_VILLAGER_COST
		| Pikeman | ElitePikeman -> cSPAWN_PIKEMAN_COST
		| Archer | EliteArcher -> cSPAWN_ARCHER_COST
		| Knight |EliteKnight -> cSPAWN_KNIGHT_COST
	

let getTeamUpgrades s c: upgrades =
	match c with
		| Red -> !(s.team_red.upgrades)
		| Blue -> !(s.team_blue.upgrades)

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

let getTeamResourceCounts (s:state) (c:color):food_count*wood_count =
	match c with
		| Red -> (!(s.team_red.food),!(s.team_red.wood))
		| Blue -> (!(s.team_blue.food),!(s.team_blue.wood))

let setTeamResourceCounts (s:state) (c:color) (food, wood) : unit =
	match c with
		| Red -> (s.team_red.food := food ;s.team_red.wood := wood)
		| Blue -> (s.team_blue.food := food ;s.team_blue.wood := wood)


let queueCollect (s:state) uid c copt tyopt: result=
   (* Check if uid is of same color*)
   match copt with
   | Some(cresult) -> 
      if c = cresult then 
         match tyopt with
          | Some(Villager) -> (
            let (u_id,u_type,u_h,u_pos) = getUnitStatus s uid in
						let u_tile = tile_of_pos u_pos in
						let resources = getResourceStatus s in
						let r = List.filter (fun (r_tile,_,_) -> 
							r_tile = u_tile) resources in
						match r with
							| [] -> Failed
							| _ -> (
								let collectqueue = !(s.gatherq) in 
								let q = Hashtbl.find collectqueue uid in
								match q with 
									| GatherQueue(gq) -> 
										(Queue.push (List.hd r) gq;Success)
									| _  -> Failed ) )
          | _ -> Failed (* uid does not exist *) 
       else Failed (* unit belongs to other team *)
   | None -> Failed (* uid does not exist *)

let queueMove (s:state) (c:color) (copt: color option) (uid:unit_id) (dest:vector):result=
	match copt with
		| None -> Failed
		| Some color -> 
			if color <> c then Failed
			else (
				if not (is_valid_pos dest ) then Failed
				else (
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
						| _ -> Failed ))


let isFreeTile s (x,y):bool =
	let r = 
		List.fold_left (fun a x -> 
			let (tile,_,_) = x in
			tile::a) [] (getResourceStatus s)
	in 
	let rbs = !(s.team_red.buildings)
	and bbs = !(s.team_blue.buildings) in
	let occupied = 
		List.fold_left (fun a (_,_,_,(x,y)) -> 
		(x,y)::(x+1,y)::(x,y+1)::(x+1,y+1)::a
		) r (rbs@bbs) in
	not (List.mem (x,y) occupied || List.mem (x+1,y) occupied
	|| List.mem (x,y+1) occupied || List.mem (x+1,y+1) occupied)

let queueBuild (s:state) (c:color) (copt:color option) (tyopt:unit_type option)
 (uid:unit_id) (b_ty:building_type):result =
   (* Check if uid is of same color*)
   match copt with
   | Some(cresult) -> 
      if c = cresult then 
         match tyopt with
          | Some(Villager) -> (
						match b_ty with
							| Barracks -> (
		            let (u_id,u_type,u_h,u_pos) = getUnitStatus s uid in
								let u_tile = tile_of_pos u_pos in
								let (food,wood) = getTeamResourceCounts s c 
								and (fr,wr)= cBARRACKS_COST in
								if (isFreeTile s u_tile) && (food>=fr) && (wood>wr)
								then (
									let buildqueue = !(s.buildq) in 
									let q = Hashtbl.find buildqueue uid in
									match q with 
										| BuildQueue(bq) -> 
											(Queue.push (u_id,b_ty) bq;
											setTeamResourceCounts s c (food-fr,wood-wr);
											Netgraphics.add_update (DoCollect (uid,c,Food,-fr));
											Netgraphics.add_update (DoCollect (uid,c,Wood,-wr));
											Success)
										| _  -> Failed)
								else Failed
								)
							| _ -> Failed

					 )
          | _ -> Failed (* uid does not exist *) 
       else Failed (* unit belongs to other team *)
   | None -> Failed (* uid does not exist *)	
	
let queueAttack (s:state) (c:color) (copt: color option)
 (tyopt:unit_type option) (uid:unit_id) (tar:attackable_object):result=
	match copt with
		| None -> Failed
		| Some color -> 
			if color <> c then Failed
			else (	
				match tyopt with
					| Some Villager -> Failed
					| _ -> (
						let q = Hashtbl.find !(s.attackq) uid in
						match q with 
							| AttackQueue(aq) -> (
								match tar with
									| Building(b) -> (
										if (getBuildingColor s b) = copt 
										then (Queue.push (uid,tar) aq; Success)
										else Failed
										)
									| Unit(u) -> (
										if (getUnitColor s u) = copt
										then (Queue.push (uid,tar) aq; Success)
										else Failed
										) )
							| _ -> Failed
						))	

let helpSpawn s c b u_ty:unit =
	let (bid,b_ty,_,b_tile) = getBuildingStatus s b in
	let (fr,wr) = getUnitCosts u_ty
	and (food,wood) = getTeamResourceCounts s c in
	setTeamResourceCounts s c (food-fr,wood-wr);
	Netgraphics.add_update (DoCollect (bid,c,Food,-fr));
	Netgraphics.add_update (DoCollect (bid,c,Wood,-wr)); 
	
	let id = next_available_id () in
	addTeamUnit s c (id,u_ty,get_unit_type_health u_ty,position_of_tile b_tile);
	Hashtbl.add !(s.movq) id (MoveQueue(Queue.create ()));
	Hashtbl.add !(s.gatherq) id (GatherQueue(Queue.create ()));
	Hashtbl.add !(s.buildq) id (BuildQueue(Queue.create ()));
	Hashtbl.add !(s.attackq) id (AttackQueue(Queue.create ()));
	Hashtbl.add !(s.cdtable) id 0.;
	Netgraphics.add_update (AddUnit (id,u_ty,position_of_tile b_tile,get_unit_type_health u_ty,c))
						
																		
let queueSpawn (s:state) (c:color) (copt:color option) (b:building_id) (u_ty:unit_type) : result =
	match copt with
		| None -> Failed
		| Some color -> 
			if color <> c then Failed
			else (
				let (bid,b_ty,_,b_tile) = getBuildingStatus s b in
				match b_ty with 
					| TownCenter -> (
						match u_ty with
							| Villager -> (helpSpawn s c b u_ty; Success)
							| _ -> Failed
						)
						| Barracks -> (
							let (up1,up2,up3) = getTeamUpgrades s c
							and age = getTeamAge s c in
							match u_ty with
								| Villager -> Failed
								| Pikeman -> (helpSpawn s c b u_ty;Success)
								| ElitePikeman -> (
									if up1 && (age=ImperialAge) then (helpSpawn s c b u_ty;Success)
									else Failed )
							  | Archer -> (
									if age=ImperialAge then (helpSpawn s c b u_ty;Success)
									else Failed )
								| EliteArcher -> (
									if up2 && (age=ImperialAge) then (helpSpawn s c b u_ty;Success)
									else Failed )
								| Knight -> (
									if age=ImperialAge then (helpSpawn s c b u_ty;Success)
									else Failed )
								| EliteKnight -> (
									if up3 && (age=ImperialAge) then (helpSpawn s c b u_ty;Success)
									else Failed )
							)
				)
						
							
let upgrade s c upgrade_type: result = 
       let (agefood,agewood)= cUPGRADE_AGE_COST in
       let (pikefood,pikewood)= cUPGRADE_PIKEMAN_COST in
       let (knightfood,knightwood) = cUPGRADE_KNIGHT_COST in
       let (archerfood,archerwood) = cUPGRADE_ARCHER_COST in
       match upgrade_type with
       | AgeUpgrade -> 
          (if c = Red then 
             (if (!(s.team_red.age)) = DarkAge then 
                (if (!(s.team_red.wood) >= agewood) && (!(s.team_red.food) >= agefood) then 
                   (s.team_red.wood:= !(s.team_red.wood) - agewood;
                    s.team_red.food:= !(s.team_red.food) - agefood;
                    s.team_red.age:= ImperialAge;
                    Netgraphics.add_update (UpgradeAge Red);
                    Success)
                else Failed) 
             else Failed) 
           else (
              (if (!(s.team_blue.age)) = DarkAge then 
                  (if (!(s.team_blue.wood) >= agewood) && (!(s.team_blue.food) >= agefood) then 
                     (s.team_blue.wood:= !(s.team_blue.wood) - agewood;
                      s.team_blue.food:= !(s.team_blue.food) - agefood;
                      s.team_blue.age:= ImperialAge;
                      Netgraphics.add_update (UpgradeAge Blue);
                      Success)
                  else Failed) 
               else Failed)))
       | UnitUpgrade(utype) ->
          (if c = Red then 
             (if !(s.team_red.age) = ImperialAge then
                let (p,a,k)= !(s.team_red.upgrades) in
                (match utype with
                 | Pikeman | ElitePikeman -> 
                    (if p then Failed 
                     else 
                        (if (!(s.team_red.wood) >= pikewood) && (!(s.team_red.food) >= pikefood) then
                           (s.team_red.wood:= !(s.team_red.wood) - pikewood;
                            s.team_red.food:= !(s.team_red.food) - pikefood;
                            s.team_red.upgrades:= (true,a,k);
                            let newunitlist= List.fold_left
                              (fun a (id,ty,h,pos) -> 
                                 if ty = Pikeman then (id,ElitePikeman,h,pos)::a 
                                 else (id,ty,h,pos)::a) [] !(s.team_red.units) in
                            s.team_red.units:= newunitlist;
                            Netgraphics.add_update (UpgradeUnit (Pikeman,Red));
                            Success)
                        else Failed))
                 | Archer |EliteArcher ->
                    (if a then Failed 
                     else 
                        (if (!(s.team_red.wood) >= archerwood) && (!(s.team_red.food) >= archerfood) then
                           (s.team_red.wood:= !(s.team_red.wood) - archerwood;
                            s.team_red.food:= !(s.team_red.food) - archerfood;
                            s.team_red.upgrades:= (p,true,k);
                            let newunitlist= List.fold_left
                              (fun a (id,ty,h,pos) -> 
                                 if ty = Archer then (id,EliteArcher,h,pos)::a 
                                 else (id,ty,h,pos)::a) [] !(s.team_red.units) in
                            s.team_red.units:= newunitlist;
                            Netgraphics.add_update (UpgradeUnit (Archer,Red));
                            Success)
                        else Failed))
                 | Knight |EliteKnight ->
                    (if k then Failed
                     else 
                        (if (!(s.team_red.wood) >= knightwood) && (!(s.team_red.food) >= knightfood) then
                           (s.team_red.wood:= !(s.team_red.wood) - knightwood;
                            s.team_red.food:= !(s.team_red.food) - knightfood;
                            s.team_red.upgrades:= (p,a,true);
                            let newunitlist= List.fold_left 
                              (fun a (id,ty,h,pos) -> 
                                 if ty = Knight then (id,EliteKnight,h,pos)::a 
                                 else (id,ty,h,pos)::a) [] !(s.team_red.units) in
                            s.team_red.units:= newunitlist;
                            Netgraphics.add_update (UpgradeUnit (Knight,Red));
                            Success)
                        else Failed))
									| _ -> Failed)
              else Failed) 
          else (
             (if !(s.team_blue.age) = ImperialAge then
                let (p,a,k)= !(s.team_blue.upgrades) in
                 (match utype with
                  | Pikeman |ElitePikeman -> 
                     (if p then Failed 
                      else 
                         (if (!(s.team_blue.wood) >= pikewood) && (!(s.team_blue.food) >= pikefood) then
                            (s.team_blue.wood:= !(s.team_blue.wood) - pikewood;
                             s.team_blue.food:= !(s.team_blue.food) - pikefood;
                             s.team_blue.upgrades:= (true,a,k);
                             let newunitlist= List.fold_left
                               (fun a (id,ty,h,pos) -> 
                                  if ty = Pikeman then (id,ElitePikeman,h,pos)::a 
                                  else (id,ty,h,pos)::a) [] !(s.team_blue.units) in
                             s.team_blue.units:= newunitlist;
                             Netgraphics.add_update (UpgradeUnit (Pikeman,Blue));
                             Success)
                         else Failed))
                  | Archer |EliteArcher ->
                     (if a then Failed 
                      else 
                         (if (!(s.team_blue.wood) >= archerwood) && (!(s.team_blue.food) >= archerfood) then
                            (s.team_blue.wood:= !(s.team_blue.wood) - archerwood;
                             s.team_blue.food:= !(s.team_blue.food) - archerfood;
                             s.team_blue.upgrades:= (p,true,k);
                             let newunitlist= List.fold_left
                               (fun a (id,ty,h,pos) -> 
                                  if ty = Archer then (id,EliteArcher,h,pos)::a 
                                  else (id,ty,h,pos)::a) [] !(s.team_blue.units) in
                             s.team_blue.units:= newunitlist;
                             Netgraphics.add_update (UpgradeUnit (Archer,Blue));
                             Success)
                         else Failed))
                  | Knight |EliteKnight ->
                     (if k then Failed
                      else 
                         (if (!(s.team_blue.wood) >= knightwood) && (!(s.team_blue.food) >= knightfood) then
                            (s.team_blue.wood:= !(s.team_blue.wood) - knightwood;
                             s.team_blue.food:= !(s.team_blue.food) - knightfood;
                             s.team_blue.upgrades:= (p,a,true);
                             let newunitlist= List.fold_left
                               (fun a (id,ty,h,pos) -> 
                                  if ty = Knight then (id,EliteKnight,h,pos)::a 
                                  else (id,ty,h,pos)::a) [] !(s.team_blue.units) in
                             s.team_blue.units:= newunitlist;
                             Netgraphics.add_update (UpgradeUnit (Knight,Blue));
                             Success)
                         else Failed))
										| _ -> Failed)
               else Failed)))	
