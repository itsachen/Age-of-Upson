open Definitions
open Util

let clients = ref []
let updates = ref []
let clients_lock = Mutex.create()
let updates_lock = Mutex.create()

let cUPDATE_SEPARATOR = "#"
let cARGUMENT_SEPARATOR = "$"
let cPOINT_SEPARATOR = "@"

let string_of_point (x,y) =
  (string_of_int (int_of_float x)) ^ cPOINT_SEPARATOR ^
  (string_of_int (int_of_float y))

let string_of_tile (x, y) =
  (string_of_int x) ^ cPOINT_SEPARATOR ^ (string_of_int y)

let combine_args = String.concat cARGUMENT_SEPARATOR

let string_of_update update =
  match update with
  | InitGraphics -> combine_args ["InitGraphics"]
	| InitFood a -> combine_args ("InitFood"::(List.map string_of_tile a))
	| InitWood a -> combine_args ("InitWood"::(List.map string_of_tile a))
  | Countdown num -> combine_args ["Countdown"; string_of_int num]
	| StopUnit (id,c,pos) -> 
			combine_args ["StopUnit"; string_of_int id; string_of_color c;string_of_point pos]
  | DisplayString (col, talk) -> 
			combine_args ["DisplayString"; string_of_color col; talk]
	| DoCollect (id,c,r,amt) -> 
			combine_args ["DoCollect";string_of_color c;string_of_resource_type r;string_of_int amt]
  | DoBuild(tile,c) -> combine_args ["DoBuild";string_of_tile tile;string_of_color c] 
	| AddUnit (uid, utype, pos, health, col) ->
      combine_args ["AddUnit"; string_of_int uid; string_of_unit_type utype;
                    string_of_point pos; string_of_int health;
                    string_of_color col]
  | AddBuilding (id, btype, tile, health, col) ->
      combine_args ["AddBuilding"; string_of_int id;
                    string_of_building_type btype; string_of_tile tile;
                    string_of_int health; string_of_color col]
  | GameOver gr -> combine_args ["GameOver"; string_of_game_result gr]
	| UpdateResource(tile,amt) -> combine_args["UpdateResource"; string_of_tile tile; string_of_int amt]
  | UpdateUnit(id, health) ->
      combine_args ["UpdateUnit"; string_of_int id; string_of_int health]
	| MoveUnit(id,lis,col) ->
			combine_args ("MoveUnit"::(string_of_int id)::(string_of_color col):: (List.map string_of_point lis))
	| UpgradeAge c -> combine_args(["UpgradeAge";string_of_color c])
	| UpgradeUnit (ut,col) -> combine_args(["UpgradeUnit";string_of_unit_type ut;string_of_color col])
	| RemoveResource tile -> combine_args (["RemoveResource";string_of_tile tile])
	| UpdateScore(c,sc) -> combine_args(["UpdateScore";string_of_color c;string_of_int sc])
	| UpdateBuilding(id,h) -> combine_args(["UpdateBuilding";string_of_int id;string_of_int h])
	| RemoveUnit id -> combine_args(["RemoveUnit";string_of_int id])
	| RemoveBuilding id -> combine_args(["RemoveBuilding";string_of_int id])
	| DoAttack id -> combine_args(["DoAttack";string_of_int id]) 
	(*| _ -> failwith "Finish adding the other graphics updates."*)

let string_of_game_result gr =
  match gr with
    Winner(c) -> string_of_color c
  | Tie -> "Tie"

let parse_updates updates =
  Mutex.lock updates_lock;
  let string_fold acc update =
    (* let _ = print_endline (string_of_update update) in*)
    acc ^ string_of_update update ^ cUPDATE_SEPARATOR in
  let sendable = List.fold_left string_fold "" updates in
  Mutex.unlock updates_lock; sendable

let init_server port =
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server Unix.SO_REUSEADDR true;
  Unix.setsockopt server Unix.SO_KEEPALIVE false;
  Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen server 100;
  server

let add_clients server =
  while true do
    let (c, a) = Unix.accept server in
    Mutex.lock clients_lock;
    print_endline "A client connected to gui server";
    clients := (Connection.server a c)::!clients;
    Mutex.unlock clients_lock;
  done

let init_single_connection port =
  let server = init_server port in
  let (c, a) = Unix.accept server in
  Mutex.lock clients_lock;
  print_endline "A client connected to gui server";
  clients := (Connection.server a c)::!clients;
  Mutex.unlock clients_lock;
  ignore(Thread.create add_clients server)

let init port = ignore(Thread.create add_clients (init_server port))

let add_update u =
  Mutex.lock updates_lock;
  updates := u::(!updates);
  Mutex.unlock updates_lock

let send u =
  Mutex.lock clients_lock;
  let parsed_updates = parse_updates u in
  clients := List.fold_left
               (fun new_clients c ->
                  if Connection.output_string c parsed_updates then
                    c::new_clients
                  else (Connection.close c; new_clients)) [] !clients;
  Mutex.unlock clients_lock

let send_update u = send [u]

let send_updates() =
  Mutex.lock updates_lock;
  let u = List.rev !updates in
  updates := [];
  Mutex.unlock updates_lock;
  send u
