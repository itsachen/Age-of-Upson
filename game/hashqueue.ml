open Definitions
open Constants
open Util
open Unit

type id= 
   UnitId of unit_id |
   BuildingId of building_id

type actqueue=
   MoveQueue of (unit_id * vector) Queue.t |
   GatherQueue of unit_id Queue.t |
   AttackQueue of (unit_id * attackable_object) Queue.t |
   BuildQueue of (unit_id * building_type) Queue.t |
   SpawnQueue of (building_id * unit_type) Queue.t

type hashqueue= (id, actqueue) Hashtbl.t

let createHashq (n: int) : hashqueue =
	Hashtbl.create n

(* Getter and setter methods *)

(* q is a ref *)
let queueCollect uid q c s: result=
   (* Check if uid is of same color*)
   let col= getTeam uid s in
   match col with
   | Some(cresult) -> 
      (if c = cresult then 
         (let ty= getType uid s in
          match ty with
          | Some(tyresult) -> 
             if ty = Villager then (
                let oldq= Hashtbl.find q uid in
                Queue.add uid oldq in
                Hashtbl.replace q uid oldq in
                Success)
             else Success
          | None -> Failed (* uid does not exist *) )
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)

let queueMove movtup q c s: result=
   (* Check if uid is of same color *)
   let col= getTeam uid s in
   match col with
   | Some(cresult) ->
      (if c = cresult then
         (let oldq= Hashtbl.find q uid in
          Queue.add movtup oldq in
          Hashtbl.replace q uid oldq in
          Success)
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)

let queueAttack atttup q c s: result=
   (* Check if uid is of same color*)
   let col= getTeam uid s in
   match col with
   | Some(cresult) -> 
      (if c = cresult then 
         (let ty= getType uid s in
          match ty with
          | Some(tyresult) -> 
             if ty = Villager then (
                let oldq= Hashtbl.find q uid in
                Queue.add uid oldq in
                Hashtbl.replace q uid oldq in
                Success)
             else Success
          | None -> Failed (* uid does not exist *) )
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)
