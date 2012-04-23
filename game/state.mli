open Definitions
open Constants
open Util

val getBuildings: game -> color -> building_data list

val getUnits: game -> color -> unit_data list

val getResources: game -> resource_data list

val getScore: game -> int

val getTime: game -> float

val getTeamStatus: state -> color -> team_data

val getUnitStatus: state -> unit_id -> unit_data

val getBuildingStatus: state -> building_id -> building_data

val getGameStatus: state -> game_data

val getResourceStatus: state -> game_data
