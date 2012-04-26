open Definitions
open Constants
open Util

val getTeamStatus: state -> color -> team_data

val getUnitStatus: state -> unit_id -> unit_data

val getBuildingStatus: state -> building_id -> building_data

val getGameStatus: state -> game_data

val getResourceStatus: state -> game_data

val setTeamScore: state -> color -> score -> unit 

val setTeamUnits: state -> color -> unit_data list -> unit 

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