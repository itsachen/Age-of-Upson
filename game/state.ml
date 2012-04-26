open Definitions
open Constants
open Util
open Hashqueue

type team = {color: color; score:score; units:unit_data list ref; 
   buildings:building_data list ref; age:age; food:food_count; wood:wood_count; 
   upgrades:upgrades}

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
