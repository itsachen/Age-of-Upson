open Definitions
open Constants
open Util

type team = {color: color; score:score; units:unit_data list ref; 
   buildings:building_data list ref; age:age; food:food_count; wood:wood_count; 
   upgrades:upgrades}

type state= {team_red: team; team_blue:team; 
   resources: resource_data list; timer: int ref}

let getBuildings (g: game) (c: color) : building_data list=
   let result= g.team_red.buildings in
   Mutex.lock g.m;
   if g.team_red.color = c then (Mutex.unlock g.m; !result)
   else (Mutex.unlock g.m; !(g.team_blue.buildings))

let getTeamStatus (s: state) (c: color): team_data=
   failwith "Not implemented yet!"

let getUnitStatus (s: state) (i: unit_id): unit_data=
   failwith "Not implemented yet!"

let getBuildingStatus (s: state) (i: building_id): building_data=
   failwith "Not implemented yet!"

let getGameStatus (s: state) : game_data=
   failwith "Not implemented yet!"

let getResourceStatus (s: state) : game_data=
   failwith "Not implemented yet!"
