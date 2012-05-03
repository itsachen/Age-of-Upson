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

val createState: unit -> state 

val getTeamStatus: state -> color -> team_data

val getUnitStatus: state -> unit_id -> unit_data

val getBuildingStatus: state -> building_id -> building_data

val getGameStatus: state -> game_data

val getResourceStatus: state -> resource_data list

val getTimer: state -> timer 	

val getCDTable: state -> (unit_id,timer) Hashtbl.t

val setTeamScore: state -> color -> score -> unit 

(*val setTeamUnits: state -> color -> unit_data list -> unit  *)
val addTeamUnit: state -> color -> unit_data -> unit

val removeTeamUnit: state -> color -> unit_id -> unit

val updateTeamUnit: state -> color -> unit_data -> unit

val setTeamBuildings: state -> color -> building_data list -> unit 

val setTeamAge: state -> color -> age -> unit 

val setTeamFood: state -> color -> food_count -> unit 

val setTeamWood: state -> color -> wood_count -> unit 

val setTeamUpgrades: state -> color -> upgrades -> unit 

val setResources: state -> resource_data list -> unit 
	
val setTimer: state -> float -> unit 
	
val setMovq: state -> hashqueue -> unit 
	
val setGatherq: state -> hashqueue -> unit 

val setAttackq: state -> hashqueue -> unit 
	
val setBuildq: state -> hashqueue -> unit 
	
val setSpawnq: state -> hashqueue -> unit 

val updateCDTable: state -> (unit_id*timer) -> unit 

val getTeam: unit_id ->  state ->  color option

val getType: unit_id ->  state ->  unit_type option

val getIsBuilding: unit_id -> state -> bool

val validAttack: state -> timer -> (unit_id*attackable_object) ->  bool 