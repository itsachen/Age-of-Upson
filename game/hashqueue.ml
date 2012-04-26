open Definitions
open Constants
open Util

type id= 
   UnitId of unit_id |
   BuildingId of building_id

type actqueue=
   MoveQueue of unit_id * vector Queue.t |
   GatherQueue of unit_id Queue.t |
   AttackQueue of unit_id * attackable_object Queue.t |
   BuildQueue of unit_id * building_type Queue.t |
   SpawnQueue of building_id * unit_type

type hashqueue= (id, actqueues) Hashtbl.t

(* Getter and setter methods *)
