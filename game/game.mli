open Definitions

type game

(* Initialize a new game *)
val initGame: unit -> game

(* initUnitsAndBuildings game initializes the units and buildings in the game
 * (the only buildings that should be initialized are town centers)
 *) 
val initUnitsAndBuildings: game -> unit

(* startGame g tells the game that all initialization is done
 * and actual play is starting
 *)
val startGame: game -> unit

(* handleAction g id a c tells the game that the unit with unit_id id has
 * attempted to perform action a, and modifies the game state to reflect that
 * attempt.
 * Returns: the appropriate Result command
 *)
val handleAction: game -> action -> color -> command

(* Teams request information with handleStatus.
 * Returns: the appropriate Data command *)
val handleStatus: game -> status -> command

(* Tick function that updates the state of the game to time t *)
val handleTime: game -> float -> game_result option
