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
let getResourceList tcloc=
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
   sortedresourcel

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

(* IS FREE TILE *)
let ift (resources:resource_data list) (g:game_data) (x,y):bool =
	if not (is_valid_tile(x,y) && is_valid_tile(x+1,y) 
		&& is_valid_tile(x,y+1) && is_valid_tile(x+1,y+1))
	then false
	else (
		let r_tiles = 
			List.fold_left (fun a x -> 
				let (tile,_,_) = x in
				tile::a) [] resources
		in 
		let (team_red,team_blue,_) = g in
		let (_,_,rbs,_,_,_,_) = team_red 
		and (_,_,bbs,_,_,_,_) = team_blue in 
		let occupied = 
			List.fold_left (fun a (_,_,_,(x,y)) -> 
			(x,y)::(x+1,y)::(x,y+1)::(x+1,y+1)::a
			) r_tiles (rbs@bbs) in
		not (List.mem (x,y) occupied || List.mem (x+1,y) occupied
		|| List.mem (x,y+1) occupied || List.mem (x+1,y+1) occupied)
	)

(* Collect thread function *)
let rec collect crazytuple=
   match crazytuple with
   (uid,h,tcloc) ->(*
   print_int (uid); print_string(": ");print_int (h);print_string ("\n");*)

   if (h <= 0) then
    ((*print_int (uid); print_string(": ");
   print_endline ("Ran out of resources");*)
      let rq= ref (queueify 
         (List.filter (fun (tl,rt,h) -> rt = Food) (getResourceList tcloc))) in
      if Queue.is_empty (!rq) then (print_endline ("No more resources");())
      else let (t,rty,hnew) = Queue.pop !rq in
      let mov= QueueMove(uid,(position_of_tile(t))) in
      let _= send_action mov 0 in
      Thread.delay 1.0;
      collect (uid,hnew,tcloc))

   else(
   let action= QueueCollect uid in
   let res= send_action action 0 in
   match res with 
   | Success -> collect (uid,(h-cRESOURCE_COLLECTED),tcloc)
   | Failed  -> 
(*print_int (uid); print_string(": ");print_endline ("Walking");*)
         Thread.delay 1.0;
collect (uid,h,tcloc)) (* Walking *)

let getWood c=
   let action= TeamStatus c in
   let foo = get_status action in
   match foo with
   TeamData (score,udl,bdl,age,food,wood,upgrades) -> print_int(wood);wood

let getFood c=
   let action= TeamStatus c in
   let foo = get_status action in
   match foo with
   TeamData (score,udl,bdl,age,food,wood,upgrades) -> food

let otherColor c=
   if c = Red then Blue else Red

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
   let (towncenterid,ty,health,loc) = List.hd bdl in
   let _= tcloc:= loc in

   let action= TeamStatus (otherColor c) in
   let foo = get_status action in
   match foo with
   TeamData (_,_,bdll,_,_,_,_) ->
   let blist= List.filter (fun (id,ty,h,tl) -> ty = TownCenter) bdll in
   let (enemytcid,_,_,otherloc) = List.hd blist in
   let xx= fst otherloc in
   let yy= snd otherloc in
   
   let villagerq= ref (queueify udl) in

   let temp= getResourceList tcloc in
   let temp2= List.filter (fun (tl,rt,h) -> rt = Wood) temp in
   let temp3= List.filter (fun (tl,rt,h) -> rt = Food) temp in

   let initwoodq= ref (queueify temp2) in
   let initfoodq= ref (queueify temp3) in
   
   (* Maybe a villager delagation thread *)
   (* One villager to wood *)
   let (t1,rty1,h1) = Queue.pop !initwoodq in
   let (u1,ty,h,p) = Queue.pop !villagerq in
   let mov1= QueueMove(u1,(position_of_tile(t1))) in

   let (t2,rty2,h2) = Queue.pop !initfoodq in
   let (u2,ty,h,p) = Queue.pop !villagerq in
   let mov2= QueueMove(u2,(position_of_tile(t2))) in 

   let (t3,rty3,h3) = Queue.pop !initfoodq in
   let (u3,ty,h,p) = Queue.pop !villagerq in
   let mov3= QueueMove(u3,(position_of_tile(t3))) in

   let _ = send_action mov1 0 in
   let _ = send_action mov2 0 in
   let _ = send_action mov3 0 in

   Thread.delay 1.0;

   let _ = Thread.create collect (u2,h2,tcloc) in
   let _ = Thread.create collect (u3,h3,tcloc) in

   (* Collect wood until enough for barracks *)
   while (((getWood c) <= snd cBARRACKS_COST) && 
      ((getFood c) <= fst cBARRACKS_COST)) do
     send_action (QueueCollect u1) 0; ()
   done;

   let a= ResourceStatus in
   let foo = get_status a in
   match foo with
   ResourceData (rdl) ->
   
   let a= GameStatus in
   let foo =  get_status a in
   match foo with
   GameData (gd) ->

   let ll= [(6,6);(6,7);(6,8);(6,9);(6,10);(6,11);(7,6);(7,7);(7,8);(7,9);
   (7,10);(7,11);(8,6);(8,7);(8,8);(8,9);(8,10);(8,11);(13,6);(3,6)] in
   let res= List.filter (fun a -> ift rdl gd a) ll in
   (* let a= QueueMove(u1,(position_of_tile(List.hd res))) in *)
   let a= QueueMove(u1,(position_of_tile((1,1)))) in
   let res= send_action a 0 in

   Thread.delay 5.0;

   let a= QueueBuild(u1,Barracks) in
   let res = send_action a 0 in

   let _ = match res with
   Success -> print_endline ("BUILT")
   | Failed -> print_endline ("FAILED YO") in

   while true do

   Thread.delay 1.0;

  done

let () = start_bot bot
