open Definitions
open Constants
open Util

type actqueue=
   MoveQueue of (unit_id * vector) Queue.t |
   GatherQueue of resource_data Queue.t |
   AttackQueue of (unit_id * attackable_object) Queue.t |
   BuildQueue of (unit_id * building_type) Queue.t |
   SpawnQueue of (building_id * unit_type) Queue.t 

type hashqueue= (int, actqueue) Hashtbl.t

let createHashq (n: int) : hashqueue =
	Hashtbl.create n

(* Getter and setter methods *)





