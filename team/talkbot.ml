open Team
open Definitions
open Constants
open Util

let _ = Random.self_init ()

let get_random_index lst =
  Random.int (List.length lst)
	
let get_random_id lst = 
  let idify (id,_,_,_) = id in
  let ids = List.map idify lst in
  List.nth ids (get_random_index ids)
	
let queueify l= 
   let qq= Queue.create () in
   let _= List.fold_left (fun a c -> Queue.add c qq; 0) (0) l in qq

(* Returns ref of resource queue, ordered in closest to TC *)
let getResourceQueue tcloc=
   let action= ResourceStatus in
   let data= get_status action in
   match data with 
   ResourceData (resourcel) ->
   let sortedresourcel= List.sort 
   (fun (t1,_,_) (t2,_,_) -> 
      let d1= distance (position_of_tile(!tcloc)) (position_of_tile(t1)) in
      let d2= distance (position_of_tile(!tcloc)) (position_of_tile(t2)) in
      if d1 < d2 then -1
      else if d1 > d2 then 1
      else 0) resourcel in
   ref (queueify sortedresourcel)

(* Collect thread function *)
let rec collect crazytuple=
   match crazytuple with
   (uid,h,tcloc,m) ->
   print_int (uid); print_string(": ");print_int (h);print_string ("\n");

   if h <= 0 then 
    (print_int (uid); print_string(": ");print_endline ("Ran out of resources");
      let rq= getResourceQueue tcloc in
      if Queue.is_empty (!rq) then (print_endline ("No more resources");())
      else let (t,rty,hnew) = Queue.pop !rq in
      let mov= QueueMove(uid,(position_of_tile(t))) in
      let _= send_action mov 0 in
      Thread.delay 1.0;
      collect (uid,hnew,tcloc,m))

   else(
   let action= QueueCollect uid in
   let res= send_action action 0 in
   match res with 
   | Success -> collect (uid,(h-cRESOURCE_COLLECTED),tcloc,m)
   | Failed  -> 
print_int (uid); print_string(": ");print_endline ("Walking");
         Thread.delay 1.0;
collect (uid,h,tcloc,m)) (* Walking *)

let count = ref 0 

(*THIS IS THE ONLY METHOD YOU NEED TO COMPLETE FOR THE BOT*)
(*Make sure to use helper funcitons*)
let bot c =
   (* Initial position of TC *)
   let tcloc= ref (0,0) in
   let action= TeamStatus c in
   let foo = get_status action in
   match foo with
   TeamData (score,udl,bdl,age,food,wood,upgrades) ->
   (* print_string(string_of_team_data((score,udl,bdl,age,food,wood))); *)
   let (towncenterid,ty,health,loc) = List.hd bdl in
   let _= tcloc:= loc in
  
   let villagerq= ref (queueify udl) in
   let resourceq= getResourceQueue tcloc in

   (* Maybe a villager delagation thread *)
   let (t1,rty1,h1) = Queue.pop !resourceq in
   let (u1,ty,h,p) = Queue.pop !villagerq in
   let mov1= QueueMove(u1,(position_of_tile(t1))) in

   let (t2,rty2,h2) = Queue.pop !resourceq in
   let (u2,ty,h,p) = Queue.pop !villagerq in
   let mov2= QueueMove(u2,(position_of_tile(t2))) in 

   let (t3,rty3,h3) = Queue.pop !resourceq in
   let (u3,ty,h,p) = Queue.pop !villagerq in
   let mov3= QueueMove(u3,(position_of_tile(t3))) in

   let res1= send_action mov1 0 in
   let res2= send_action mov2 0 in
   let res3= send_action mov3 0 in

   let cm = Mutex.create () in
   let _ = Thread.create collect (u1,h1,tcloc,cm) in
   let _ = Thread.create collect (u2,h2,tcloc,cm) in
   let _ = Thread.create collect (u3,h3,tcloc,cm) in

   while true do
   
   Thread.delay 1.0 
(*
    let talk_action = Talk("Talk: " ^ (string_of_int !count)) in
		let audio_action = Talk(string_of_int !count) in
		count := (mod) (!count + 1) 40;
		let b = Random.int 100 in
		let action = if b < 10 then audio_action else talk_action in
    let res = send_action action 0 in
    let _ = match res with
      | Success -> print_endline ("Talk Success!")
      | Failed  -> print_endline ("Talk Failed") in
    Thread.delay 1.0
*)
  done

let () = start_bot bot
