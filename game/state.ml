open Definitions
open Constants
open Util
open Hashqueue

type team = {color: color; score:score ref; units:unit_data list ref; 
   buildings:building_data list ref; age:age ref; food:food_count ref; wood:wood_count ref; 
   upgrades:upgrades ref}

type state= {team_red: team; team_blue:team; 
   resources: resource_data list; timer: float ref; 
   movq: hashqueue ref; gatherq: hashqueue ref; attackq: hashqueue ref; 
   buildq: hashqueue ref; spawnq: hashqueue ref}

let getTeamStatus (s: state) (c: color): team_data=
   match c with
   | Red -> 
      (let r= s.team_red in 
         (r.score,r.units,r.buildings,r.age,r.food,r.wood,r.upgrades))
   | Blue ->
      (let b= s.team_blue in 
         (b.score,b.units,b.buildings,b.age,b.food,b.wood,b.upgrades))
  

(* @# WHAT HAPPENS IF UNIT_ID OR BUILDING_ID IS NOT IS NOT THERE *)
let getUnitStatus (s: state) (i: unit_id): unit_data=
   let foo= List.fold_left
      (fun a c ->
         match c with
         | (id,ty,h,p) -> if id = i then c else a) 
      (-1,Villager,-1,(-1.0,-1.0)) s.team_red.units in
   if foo = (-1,Villager,-1,(-1.0,-1.0)) then
      (List.fold_left 
         (fun a c ->
            match c with
            | (id,ty,h,p) -> if id = i then c else a) 
      (-1,Villager,-1,(-1.0,-1.0)) s.team_blue.units)
   else foo

let getBuildingStatus (s: state) (i: building_id): building_data=
   let foo= List.fold_left
      (fun a c ->
         match c with
         | (id,ty,h,ti) -> if id = i then c else a) 
      (-1,Barracks,-1,(-1,-1)) s.team_red.buildings in
   if foo = (-1,Barracks,-1,(-1,-1)) then
      (List.fold_left 
         (fun a c -> 
            match c with
            | (id,ty,h,ti) -> if id = i then c else a) 
      (-1,Barracks,-1,(-1,-1)) s.team_blue.buildings)
   else foo

let getGameStatus (s: state) : game_data=
   (s.team_red,s.team_blue.s.timer,s.timer)

(* Double check? *)
let getResourceStatus (s: state) : resource_data list=
   s.resources
	
	
let setTeamScore (s:state) (c:color) (sc: score):unit =
	match c with
		| Red -> s.team_red.score := sc; ()
		| Blue -> s.team_blue.score := sc; ()

let setTeamUnits (s:state) (c:color) (u: unit_data list):unit =
	match c with
		| Red -> s.team_red.units := u; ()
		| Blue -> s.team_blue.units := u; ()

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
	
	
			
	
