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

(*c is a color type already*)

(* q is a ref *)
(*let queueCollect (s:state) uid c copt tyopt: result=
   (* Check if uid is of same color*)
   match copt with
   | Some(cresult) -> 
      (if c = cresult then 
         (match tyopt with
          | Some(Villager) -> 
            let (u_id,u_type,u_h,u_pos) = getUnitStatus s unit_id in
						let u_tile = tile_of_pos u_pos in
						let resources = getResourceStatus s in
						let r = List.filter (fun (r_tile,_,_) -> 
							r_tile = u_tile) resources in
						match r with
							| [] -> Failed
							| _ -> (
								let (r_tile,r_ty,count)= List.hd r in
								let inc = 
									match (getTeamAge s c) with
										| DarkAge -> cRESOURCE_COLLECTED
										| ImperialAge -> cADVANCED_RESOURCE_COLLECTED
								in let r_inc = 
									if r_inc <= count then r_inc
									else count
								in updateResource s (r_tile,r_ty,count-r_inc);
								Netgraphics.add_update (DoCollect (u_id,c,r_ty,r_inc));
								addTeamScore s c r_inc;
								Netgraphics.add_update (UpdateScore (c,getTeamScore s c))
								
								)
          | _ -> Failed (* uid does not exist *) )
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)



let queueMove movtup q c copt: result=
   (* Check if uid is of same color *)
   match copt with
   | Some(cresult) ->
      (if c = cresult then
         (let oldq= Hashtbl.find q (fst movtup) in
          match oldq with
          MoveQueue(qq) ->(
          Queue.add movtup qq;
          Hashtbl.replace q (fst movtup) (MoveQueue(qq));
          Success)
          | _ -> Success)
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)
*)
let queueAttack atttup q c copt tyopt: result=
   (* Check if uid is of same color*)
   match copt with
   | Some(cresult) -> 
      (if c = cresult then 
         (match tyopt with
          | Some(tyresult) -> 
             if tyresult <> Villager then (
                let oldq= Hashtbl.find q (fst atttup) in
                match oldq with
                AttackQueue(qq) ->(
                Queue.add atttup qq;
                Hashtbl.replace q (fst atttup) (AttackQueue(qq));
                Success)
                | _ -> Success)
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
             match oldq with
             SpawnQueue(qq) -> (
             Queue.add spawntup qq;
             Hashtbl.replace q (fst spawntup) (SpawnQueue(qq));
             Success)
             | _ -> Success)
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
                Hashtbl.replace q uid (AttackQueue(emptqueue));
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
          Hashtbl.replace q uid (MoveQueue(emptqueue));
          Success)
       else Failed (* unit belongs to other team *))
   | None -> Failed (* uid does not exist *)
