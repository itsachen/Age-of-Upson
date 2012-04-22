open Constants
open Definitions

let id_lock = Mutex.create ()
let id = ref 0

(* use next_available_id when assigning IDs to units and buildings *)
let next_available_id () =
  let _ = Mutex.lock id_lock in
  let the_id = !id in
  let _ = id := !id + 1 in
  Mutex.unlock id_lock;
  the_id

let is_valid_pos (x, y) =
  if (x < 0.0 || x > cBOARD_WIDTH) then false
  else if (y < 0.0 || y > cBOARD_HEIGHT) then false
  else true

let is_valid_tile (r, c) =
  if (r < 0 || r >= cNUM_Y_TILES) then false
  else if (c < 0 || c >= cNUM_X_TILES) then false
  else true

(* helpful vector operations *)
let distance (x1, y1) (x2, y2) =
	sqrt ((x2-.x1)*.(x2-.x1) +. (y2-.y1)*.(y2-.y1))
	
let length (x, y) = (sqrt (x*.x +. y*.y))

let normalize (x, y) =
	let len = length (x, y) in
	(x /. len, y /. len)

let dot (x1, y1) (x2, y2) = x1*.x2 +. y1*.y2

(* converts a range into pixels *)
let length_of_range (range : float) : float =
  let tile_length = length (cTILE_WIDTH, cTILE_HEIGHT) in
  range *. tile_length

(* determines if a circle with center (cx, cy) and radius r intersects
 * a line segment starting at (x1, y1) and ending at (x2, y2) *)
let circle_line_intersection (cx, cy) r (x1, y1) (x2, y2) : bool =
  let d = (x2-.x1, y2-.y1) in
  let f = (x1-.cx, y1-.cy) in
  let a = dot d d in
  let b = 2. *. (dot d f) in
  let c = (dot f f) -. r*.r in
  let discriminant = b*.b -. 4.*.a*.c in
  if discriminant < 0. then false
  else
    let t1 = (-.b +. (sqrt discriminant)) /. (2.*.a) in
    let t2 = (-.b -. (sqrt discriminant)) /. (2.*.a) in
    (t1 >= 0. && t1 <= 1.) || (t2 >= 0. && t2 <= 1.)

(* determines if a circle with center c and radius r intersects
 * a square with top-left point (x1, y1) and bottom-right point (x2, y2) *)
let circle_square_intersection ((cx, cy) as c) r (x1, y1) (x2, y2) : bool =
  let top_left = (x1, y1) in
  let top_right = (x2, y1) in
  let bottom_left = (x1, y2) in
  let bottom_right = (x2, y2) in
  let check_intersect = circle_line_intersection c r in
  (* check intersection with each edge of the square *)
  let check_left = check_intersect bottom_left top_left in
  let check_top = check_intersect top_left top_right in
  let check_right = check_intersect bottom_right top_right in
  let check_bottom = check_intersect bottom_right bottom_left in
  (* if the center of the circle is contained in the square then we count it as
   * an intersection *)
  let check_contains = (cx >= x1 && cx <= x2 && cy >= y1 && cy <= y2) in
  check_left || check_top || check_right || check_bottom || check_contains

let string_of_color c =
  match c with
  | Red -> "Red"
  | Blue -> "Blue"

let string_of_resource_type = function
	| Food -> "Food"
	| Wood -> "Wood"

let string_of_game_result game_res =
  match game_res with
  | Winner c -> string_of_color c
  | Tie -> "Tie"

let get_building_type_health btype =
  match btype with
  | TownCenter -> cTOWNCENTER_HEALTH
  | Barracks -> cBARRACKS_HEALTH

let get_unit_type_health utype =
  match utype with
  | Villager -> cVILLAGER_HEALTH
  | Knight -> cKNIGHT_HEALTH
  | Archer -> cARCHER_HEALTH
  | Pikeman -> cPIKEMAN_HEALTH
  | EliteKnight -> cELITE_KNIGHT_HEALTH
  | EliteArcher -> cELITE_ARCHER_HEALTH
  | ElitePikeman -> cELITE_PIKEMAN_HEALTH

let get_unit_type_cooldown utype =
  match utype with
  | Villager -> cVILLAGER_COOLDOWN
  | Knight -> cKNIGHT_COOLDOWN
  | Archer -> cARCHER_COOLDOWN
  | Pikeman -> cPIKEMAN_COOLDOWN
  | EliteKnight -> cELITE_KNIGHT_COOLDOWN
  | EliteArcher -> cELITE_ARCHER_COOLDOWN
  | ElitePikeman -> cELITE_PIKEMAN_COOLDOWN

let get_unit_type_range utype =
  match utype with
  | Villager -> failwith "Villager's can't attack. They don't have a range."
  | Knight -> cKNIGHT_RANGE
  | Archer -> cARCHER_RANGE
  | Pikeman -> cPIKEMAN_RANGE
  | EliteKnight -> cELITE_KNIGHT_RANGE
  | EliteArcher -> cELITE_ARCHER_RANGE
  | ElitePikeman -> cELITE_PIKEMAN_RANGE

let get_unit_type_attack_damage utype =
  match utype with
  | Villager -> failwith "Villager's can't attack."
  | Knight -> cKNIGHT_ATTACK
  | Archer -> cARCHER_ATTACK
  | Pikeman -> cPIKEMAN_ATTACK
  | EliteKnight -> cELITE_KNIGHT_ATTACK
  | EliteArcher -> cELITE_ARCHER_ATTACK
  | ElitePikeman -> cELITE_PIKEMAN_ATTACK

let get_unit_type_speed utype =
  match utype with
  | Villager -> cVILLAGER_SPEED
  | Knight -> cKNIGHT_SPEED
  | Archer -> cARCHER_SPEED
  | Pikeman -> cPIKEMAN_SPEED
  | EliteKnight -> cELITE_KNIGHT_SPEED
  | EliteArcher -> cELITE_ARCHER_SPEED
  | ElitePikeman -> cELITE_PIKEMAN_SPEED

let string_of_building_type btype =
  match btype with
  | TownCenter-> "TownCenter"
  | Barracks -> "Barracks"

let string_of_unit_type utype =
  match utype with
  | Villager -> "Villager"
  | Knight -> "Knight"
  | Archer -> "Archer"
  | Pikeman -> "Pikeman"
  | EliteKnight -> "Elite Knight"
  | EliteArcher -> "Elite Archer"
  | ElitePikeman -> "Elite Pikeman"

let print_unit_type utype = print_endline (string_of_unit_type utype)

let string_of_status = function
		| UnitStatus unit_id -> "Unit Status of " ^ (string_of_int(unit_id)) 
		| BuildingStatus building_id ->
			"Building Status of " ^ (string_of_int(building_id))
		| TeamStatus color -> "Team Status"
		| ResourceStatus -> "ResourceStatus"
		| GameStatus -> "GameStatus"

let string_of_building_type = function
	| TownCenter -> "TownCenter"
	| Barracks -> "Barracks"

let string_of_unit_type = function
	| Villager -> "Villager"
	| Knight -> "Knight"
	| EliteKnight -> "EliteKnight"
	| Archer -> "Archer"
	| EliteArcher -> "EliteArcher"
	| Pikeman -> "Pikeman"
	| ElitePikeman -> "ElitePikeman"

let string_of_attackable_object = function
	| Building id -> "Building" ^ (string_of_int id)
	| Unit id -> "Unit" ^ (string_of_int id)

let string_of_vector (x,y) =
	"("^(string_of_float x)^","^(string_of_float y)^")"

let string_of_action = function
	| QueueAttack(id, attackable_object) ->
		"Queue Attack: " ^ (string_of_int id) ^
		(string_of_attackable_object attackable_object)
	| QueueCollect id -> "Queue Collect: " ^ (string_of_int id)
  | QueueMove(id,vector) ->
		"Queue Move: " ^ (string_of_int id) ^ (string_of_vector vector)
  | QueueBuild(id, building_type) ->
		"Queue Build: " ^ (string_of_int id) ^
		(string_of_building_type building_type)
	| QueueSpawn(id, unit_type) ->
		"Queue Spawn: " ^ (string_of_int id) ^ (string_of_unit_type unit_type)
	| ClearAttack id -> "Clear Attack: " ^ (string_of_int id)
	| Upgrade upgrade_type -> "Upgrade"
	| Talk string -> "Talk: " ^ string
	| ClearMove id -> "ClearMove " ^ (string_of_int id)
	
let string_of_age = function
	| DarkAge -> "DarkAge"
	| ImperialAge -> "ImperialAge"
	
let string_of_point (x,y) =
  "(" ^ (string_of_int (int_of_float x)) ^ "," ^ 
	(string_of_int (int_of_float y)) ^ ")"

let string_of_tile (x,y) = "("^(string_of_int x)^","^(string_of_int y)^")"	
	
let string_of_building_data (building_id,building_type,health,tile) = 
		"{id:"^(string_of_int building_id)^",type:"^
		(string_of_building_type building_type)^",health:"^(string_of_int health)^
		",tile:"^(string_of_tile tile) ^ "} "
	
let string_of_building_data_list building_data_list =
	List.fold_left (fun a v -> (string_of_building_data v) ^ a) "" building_data_list

let string_of_unit_data (unit_id,unit_type,health,position) = 
		"{id:"^(string_of_int unit_id)^",type:"^(string_of_unit_type unit_type)^
		",health:"^(string_of_int health)^",pos:"^
		(string_of_vector position)^ "} "

let string_of_unit_data_list unit_data_list =
	List.fold_left (fun a v -> (string_of_unit_data v) ^ a) "" unit_data_list

let string_of_team_data (score,unit_data_list,building_data_list,age,food,wood) =
	"("^(string_of_int  score)^", "^(string_of_unit_data_list unit_data_list)^
	", "^(string_of_building_data_list building_data_list)^(string_of_age age)^
	(string_of_int food)^(string_of_int wood)^")"

let tile_of_pos ((xpos,ypos):float*float) : int*int =
	let col = int_of_float (xpos/.cTILE_WIDTH) in
	let row = int_of_float (ypos/.cTILE_HEIGHT) - 2 in
	(row,col)
	
let position_of_tile (row,col) : float*float=
	let xpos = (float_of_int col)*.cBOARD_WIDTH/.(float_of_int cNUM_X_TILES) +. cTILE_WIDTH/.2.0 in
	let ypos = (float_of_int row)*.cBOARD_HEIGHT/.(float_of_int cNUM_Y_TILES) +. cTILE_HEIGHT*.2.0 in
	(xpos,ypos)
