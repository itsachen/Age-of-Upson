type connection

(* init addr retries attempts to initialize a connection with addr, retrying at
 * most retries times. If initialization succeeds, all subsequent outputs
 * using this connection will be retried up to retries times.
 * Returns: Some(c), where c is the successfully initialized connection to addr
 *          None, if initialization failed
 *)
val init : Unix.sockaddr -> int -> connection option

(* input conn blocks until a value is present on conn, or the connection closes
 * Returns: Some(v), if the read on conn succeeded, where v is the value
 *          present on conn.
 *          None, otherwise.
 *)
val input : connection -> 'a option

(* output conn value sends value through conn, retrying up to retries times.
 * Returns: whether the output succeeded.
 *)
val output : connection -> 'a -> bool

(* output_string conn value sends value through conn, retrying up to retries
 * times.
 * Returns: whether the output succeeded.
 *)
val output_string : connection -> string -> bool

(* close conn closes conn and all associated file descriptors *)
val close : connection -> unit

(* address conn returns the address for the connection *)
val address : connection -> Unix.sockaddr

(* Students: don't call this.
 *
 * Generates a connection object based on an already opened socket.
 *)
val server : Unix.sockaddr -> Unix.file_descr -> connection
