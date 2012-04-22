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
	
let count = ref 0 

(*THIS IS THE ONLY METHOD YOU NEED TO COMPLETE FOR THE BOT*)
(*Make sure to use helper funcitons*)
let bot c =
  while true do
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
  done

let () = start_bot bot
