(* Initializes the server and waits until a single gui client has connected *)
val init_single_connection: int -> unit
 
(* init port initializes the gui server, listening on the provided port *)
val init: int -> unit

(* add_update u adds a graphical update to a list of updates to send later *)
val add_update: Definitions.update -> unit

(* send_update u immediately sends a graphical update *)
val send_update: Definitions.update -> unit

(* send_updates() sends off all graphical updates added with add_update since
 * the last call to send_updates
 * Called regularly by Server, so students, you DON'T need to call this. *)
val send_updates: unit -> unit
