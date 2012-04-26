open Definitions
open Constants
open Util

val getTeamStatus: state -> color -> team_data

val getUnitStatus: state -> unit_id -> unit_data

val getBuildingStatus: state -> building_id -> building_data

val getGameStatus: state -> game_data

val getResourceStatus: state -> game_data
