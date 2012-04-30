open Definitions
open Constants
open Util

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

(*c is a color type already*)

(* q is a ref *)
let queueCollect uid q c copt tyopt: result=
   (* Check if uid is of same color*)
   match copt with
   | Some(cresult) -> 
      (if c = cresult then 
         (match tyopt with
          | Some(tyresult) -> 
             if tyresult = Villager then (
                let oldq= Hashtbl.find q uid in
                Queue.add uid oldq;
                Hashtbl.replace q uid oldq;
                Success)
             else Success
          | None -> Failed (* uid does not exist *) )
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)

let queueMove movtup q c copt: result=
   (* Check if uid is of same color *)
   match copt with
   | Some(cresult) ->
      (if c = cresult then
         (let oldq= Hashtbl.find q (fst movtup) in
          Queue.add movtup oldq;
          Hashtbl.replace q (fst movtup) oldq;
          Success)
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)

let queueAttack atttup q c copt tyopt: result=
   (* Check if uid is of same color*)
   match copt with
   | Some(cresult) -> 
      (if c = cresult then 
         (match tyopt with
          | Some(tyresult) -> 
             if tyresult <> Villager then (
                let oldq= Hashtbl.find q (fst atttup) in
                Queue.add atttup oldq;
                Hashtbl.replace q (fst atttup) oldq;
                Success)
             else Success
          | None -> Failed (* uid does not exist *) )
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)

let queueBuild buildtup q c copt tyopt: result=
   (* Check if uid is of same color*)
   match copt with
   | Some(cresult) -> 
      (if c = cresult then 
         (match tyopt with
          | Some(tyresult) -> 
             if tyresult = Villager then (
                let oldq= Hashtbl.find q (fst buildtup) in
                Queue.add buildtup oldq;
                Hashtbl.replace q (fst buildtup) oldq;
                Success)
             else Success
          | None -> Failed (* uid does not exist *) )
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)

let queueSpawn spawntup q c copt isbuild: result=
   (* Check if uid is of same color*)
   match copt with
   | Some(cresult) -> 
      (if c = cresult then 
         (if isbuild then (
             let oldq= Hashtbl.find q (fst spawntup) in
             Queue.add spawntup oldq;
             Hashtbl.replace q (fst spawntup) oldq;
             Success)
          else Success (* Not a building, fool *))
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)

let clearAttack uid q c copt tyopt: result=
   (* Check if uid is of same color*)
   match copt with
   | Some(cresult) -> 
      (if c = cresult then 
         (match tyopt with
          | Some(tyresult) -> 
             if tyresult <> Villager then (
                let emptqueue= Queue.create () in
                Hashtbl.replace q uid emptqueue;
                Success)
             else Success
          | None -> Failed (* uid does not exist *) )
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)

let clearMove uid q c copt: result=
   (* Check if uid is of same color *)
   match copt with
   | Some(cresult) ->
      (if c = cresult then
         (let emptqueue= Queue.create () in 
          Hashtbl.replace q uid emptqueue;
          Success)
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)
