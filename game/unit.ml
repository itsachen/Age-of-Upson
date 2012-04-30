open Definitions
open Constants
open Util
open State

(* CHANGE BOTH TO LIST.FIND_ALL *)
(* Returns the team that the UID is. *)
let getTeam uid s: color option=
   let foo= List.fold_left
      (fun a c ->
         match c with
         | (id,ty,h,p) -> if id = uid then c else a) 
      (-1,Villager,-1,(-1.0,-1.0)) !(s.team_red.units) in
   if foo = (-1,Villager,-1,(-1.0,-1.0)) then
      (let foo2= List.fold_left 
         (fun a c ->
            match c with
            | (id,ty,h,p) -> if id = uid then c else a) 
      (-1,Villager,-1,(-1.0,-1.0)) !(s.team_blue.units) in
      if foo2= (-1,Villager,-1,(-1.0,-1.0)) then None
      else Some(Blue))
   else Some(Red)

let getType uid s: unit_type option=
   let foo= List.fold_left
      (fun a c ->
         match c with
         | (id,ty,h,p) -> if id = uid then c else a) 
      (-1,Villager,-1,(-1.0,-1.0)) !(s.team_red.units) in
   if foo = (-1,Villager,-1,(-1.0,-1.0)) then
      (let foo2= List.fold_left 
         (fun a c ->
            match c with
            | (id,ty,h,p) -> if id = uid then c else a) 
      (-1,Villager,-1,(-1.0,-1.0)) !(s.team_blue.units) in
      if foo2= (-1,Villager,-1,(-1.0,-1.0)) then None
      else let (_,ty,_,_)= foo2 in Some(ty))
   else let (_,ty,_,_)= foo in Some(ty)
