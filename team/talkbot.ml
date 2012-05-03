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

let count = ref 0 

(*THIS IS THE ONLY METHOD YOU NEED TO COMPLETE FOR THE BOT*)
(*Make sure to use helper funcitons*)
let bot c =
   let tcloc= ref (0,0) in
   (* Initial position of TC *)
   let action= TeamStatus c in
   let (score,udl,bdl,age,food,wood,upgrades) = get_status action in
   let (towncenterid,ty,health,loc) = List.hd bdl in
   tcloc:= loc in
  
   let action= ResourceStatus in
   let resourcel= get_status action in
   let sortedresourcel= List.sort 
   (fun (t1,_,_) (t2,_,_) -> 
      let d1= distance (position_of_tile(!tcloc)) (position_of_tile(t1)) in
      let d2= distance (position_of_tile(!tcloc)) (position_of_tile(t2)) in
      if d1 < d2 then -1
      else if d1 > d2 then 1
      else 0) resourcel in

   let villagerq= queueify udl in
   let resourceq= queueify sortedresourcel in

   let (t1,rty,i) = Queue.pop resourceq in
   let (u1,ty,h,p) = Queue.pop villagerq in
   let mov1= QueueMove(u1,(position_of_tile(t1)) in

   let (t2,rty,i) = Queue.pop resourceq in
   let (u2,ty,h,p) = Queue.pop villagerq in
   let mov2= QueueMove(u2,(position_of_tile(t2)) in 

   let (t3,rty,i) = Queue.pop resourceq in
   let (u3,ty,h,p) = Queue.pop villagerq in
   let mov3= QueueMove(u3,(position_of_tile(t3)) in

   let res1= send_action mov1 0 in
   let res2= send_action mov2 0 in
   let res3= send_action mov3 0 in

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
