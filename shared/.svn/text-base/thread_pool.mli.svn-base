(* Thread Pool - on creation a set of worker threads are started.
 * Work to be done is added to the thread pool, and an available worker
 * thread gets to that work when it can. There are no guarantees about
 * when the work will be done, or win what order. *)

type pool

(* No_workers exception is thrown if addwork is called when the
 * threadpool is being shut down. The work is not added. *)
exception No_workers

(* create a thread pool with the specified number of worker threads *)
val create : int -> pool

(* add work to the pool, where work is any unit->unit function *)
val addwork : (unit -> unit) -> pool -> unit

(* destroy a thread pool, stopping all the threads once all work
 * in the queue has been completed. *)
val destroy : pool -> unit

val empty_queue : pool -> unit
