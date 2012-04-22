(* Implementation as a 4-tuple of: thread count, mutable queue of
 * functions waiting to be run, mutex protecting thread count and queue,
 * and condition variable signaling work to be done. *)
type pool = (int ref * (unit -> unit) Queue.t * Mutex.t * Condition.t)

exception No_workers

(* Loop run by each worker thread, consumes work from the queue
 * following producer/consumer pattern with a condition variable,
 * described previously. *)
let dowork (tp : pool) =
  match tp with (nworkers, q, m, c) ->
    Mutex.lock m;
    (* When nworkers <= 0, it means the thread pool is being
     * destroyed. If that is true and there is also no work left to do
     * then stop looping and exit, decrementing worker count. *)
    while (!nworkers > 0) || (Queue.length q > 0) do
      (* In normal operation where nworkers > 0 wait for stuff in the queue *)
      while (!nworkers > 0) && (Queue.length q = 0) do
        Condition.wait c m
      done;
      (* Verify something in the queue rather than we are now being
       * shut down, then get the work from the queue, unlock the
       * mutex, do the work and relock the mutex before looping
       * back. *)
      if Queue.length q > 0 then
        let f = Queue.take q in
        Mutex.unlock m;
        (* Don't let an exception in the work, f, kill the thread.
         * Just catch it and go on. *)
        (try ignore (f())
         with e ->
           print_endline ("Work in thread pool resulted in exception : " ^
                          (Printexc.to_string e)));
        Mutex.lock m
    done;
    (* A worker thread exits when the pool is being shut down.  It
     * decrements the worker count which when all threads are
     * finished should be -n, where n was the number of threads in
     * the pool (counts down from 0). *)
    nworkers := !nworkers-1;
    Mutex.unlock m

(* Creates the counter, queue, mutex used to protect these mutable
 * variables, and condition used to signal worker threads.  Creates
 * the specified number of threads, each running the dowork
 * loop. Returns the newly created threadpool. *)
let create (size : int) : pool =
  if size < 1 then raise (Failure "Tpool create needs at least one thread.")
  else
    let tp = (ref 0, Queue.create(), Mutex.create(), Condition.create()) in
    let (nworkers, _, m, _) = tp in
    Mutex.lock m;
    while !nworkers < size do
      ignore (Thread.create dowork tp);
      nworkers := !nworkers + 1
    done;
    Mutex.unlock m;
    tp

(* Adds the specified function as work to the thread pool's work
 * queue, following producer/consumer pattern using condition
 * variable described previously.  Note that work need not run in the
 * order it was added (it will be dequeued in order but the threads
 * need not run sequentially). *)
let addwork (f : unit->unit) (tp : pool) : unit =
  let (nworkers, q, m, c) = tp in
  Mutex.lock m;
  if !nworkers < 1 then (Mutex.unlock m; raise No_workers)
  else
    (Queue.add f q;
     Condition.signal c;
     Mutex.unlock m)

(* Waits for all threads to exit, each of which decrements nworkers
 * from 0, so when it gets to -n we are done.  Used in destroy. *)
let rec done_wait (tp : pool) (n : int) =
  let (nworkers, _, m, c) = tp in
  Mutex.lock m;
  if !nworkers <= (- n) then Mutex.unlock m
  else
    (print_string
       ("Tpool destroy, still waiting for " ^ string_of_int(n + !nworkers) ^
        " threads.\n");
     flush stdout;
     Mutex.unlock m;
     Thread.delay 0.1;
     done_wait tp n)

(* Destroys the thread pool by setting nworkers to zero, then wakes
 * all threads waiting for the worker condition variable so that they
 * can exit if they find no work in the queue remaining to be
 * done. *)
let destroy (tp : pool) : unit =
  let (nworkers, q, m, c) = tp in
  Mutex.lock m;
  let n = !nworkers in
  nworkers := 0;
  Condition.broadcast c;
  Mutex.unlock m;
  done_wait tp n

let empty_queue (tp : pool) : unit =
  let (nworkers, q, m, c) = tp in
  Mutex.lock m; Queue.clear q; Mutex.unlock m
