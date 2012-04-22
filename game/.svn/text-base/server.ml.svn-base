open Constants
open Definitions
open Util

let num_players = cNUM_TEAMS

let valOf x = match x with Some(y) -> y | None -> failwith "Tried to valOf None"

let listen port numConnections =
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server Unix.SO_REUSEADDR true;
  Unix.setsockopt server Unix.SO_KEEPALIVE false;
  Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen server numConnections;
  server

(* Generic thread safety stuff *)
let getTSValue (r: 'a ref) (m: Mutex.t) : 'a =
  Mutex.lock m;
  let v = !r in
  Mutex.unlock m;
  v

let performTSRefOp (f: 'a ref -> 'b) (r: 'a ref) (m: Mutex.t) : 'b =
  Mutex.lock m;
  let v = f r in
  Mutex.unlock m;
  v

let performTSOp (f: 'a -> 'b) (r: 'a ref) (m: Mutex.t) : 'b =
  Mutex.lock m;
  let v = f (!r) in
  Mutex.unlock m;
  v

let startGameServer port guiPort =
  let _ = Netgraphics.init guiPort in
  let numConnections = 100 in
  let server = listen port numConnections in
  let shutdown = ref false in
  let shutdownMutex = Mutex.create () in

  let players = ref [] in
  let playersMutex = Mutex.create () in

  let currentGameThreads = ref [] in
  let currentGameThreadsMutex = Mutex.create () in

  let rec acceptPlayers () =
    let _ = try
      let (c, a) = Unix.accept server in
      if getTSValue shutdown shutdownMutex then () else
      let client = Connection.server a c in
      match Connection.input client with
        Some(Control(GameRequest)) -> 
            performTSRefOp (fun l -> players := (!players)@[client]) 
                                    players playersMutex;
            print_endline "A client has connected."
      | _ -> Connection.close client
    with e -> print_endline ("Listening for a client connection 
                          resulted in exception : " ^ Printexc.to_string e) in
    acceptPlayers() in
  (* Create a thread that accepts incoming connections from players.
   * This thread can be safely killed if shutdown is
   * set and the thread is currently accepting. *)
  let _ = Thread.create acceptPlayers () in

  let rec startNewGame () =
    let _ = print_endline "Preparing to start new game..." in
    let gameResult = ref None in
    let gameResultMutex = Mutex.create () in
    let lostConn = ref None in
    let lostConnMutex = Mutex.create () in
    let rec waitAndPop () =
      if performTSOp List.length players playersMutex = 0 then
        (Thread.delay 0.5; waitAndPop())
      (* this is the only call to pop in the program so its safe
      * to do this atomically...*)
      else performTSRefOp
             (fun l -> match !l with
                  h::t -> (l := t; h)
                | _ -> failwith "unknown error")
             players playersMutex in

    let rec initPlayer c =
      let p = waitAndPop() in
      if Connection.output p (Control(Team(c))) then p else initPlayer c in

    let p1 = initPlayer Red in
    let _ = print_endline "Red player initialized" in
    let p2 = initPlayer Blue in
    let _ = print_endline "Blue player initialized" in

    let game = Game.initGame () in
    Game.initUnitsAndBuildings game;
    Netgraphics.send_updates ();

    let rec countdown i =
      if i > 0 then print_endline ("  "^string_of_int i) ;
      if i = 0 then print_endline "  GO" ;
      if i >= 0 then (Netgraphics.send_update(Countdown i);
        Thread.delay 1.0 ; countdown (i-1)) in

    countdown 3;
    print_endline "Starting game.";
    Game.startGame game;
    let _ = Connection.output p1 (Control(GameStart)) in
    let _ = Connection.output p2 (Control(GameStart)) in

    (* Handle inbound messages from the connected clients *)
    let rec handleConnection c client =
      let cont = ref true in
      while !cont do
      (try (
        cont :=
          match Connection.input client with
              Some(Action(a)) ->
                let res = Game.handleAction game a c in
                Connection.output client (res)
            | Some(Status s) ->
                let res = Game.handleStatus game s in
                Connection.output client (res)
            | Some(_) -> Connection.output client
                           (Error "Wrong command message sent to server.")
            | _ -> (performTSRefOp (fun lc -> lc := Some c)
                      lostConn lostConnMutex; false))
      with e -> print_endline (Printexc.to_string e);
                performTSRefOp (fun lc -> lc := Some c)
                  lostConn lostConnMutex; cont := false)
      done in
    let p1Thread = Thread.create (handleConnection Red) p1 in
    let p2Thread = Thread.create (handleConnection Blue) p2 in
    performTSRefOp (fun lr -> lr := p1Thread::p2Thread::!lr)
      currentGameThreads currentGameThreadsMutex;

    (* Handle time updates in the game *)
    let rec handleTime () =
      let result = Game.handleTime game (Unix.gettimeofday()) in
      Thread.delay cUPDATE_TIME;
      match result with
        None -> if getTSValue shutdown shutdownMutex then () else handleTime ()
      | Some c -> Netgraphics.add_update(GameOver(c));
                  Netgraphics.send_updates();
                  performTSRefOp (fun grr -> grr := Some c)
                    gameResult gameResultMutex in
		let _ = Thread.create handleTime () in
		
    (* Handle gui updates in the game *)
    let rec handleGuiUpdates () =
      Netgraphics.send_updates();
      Thread.delay cUPDATE_TIME;
      if getTSValue shutdown shutdownMutex then () else handleGuiUpdates () in

    (* NOTE: this thread should shutdown on its own *)
    let _ = Thread.create handleGuiUpdates () in

    let rec waitForGameOver() =
      let sd = getTSValue shutdown shutdownMutex in
      let gro = getTSValue gameResult gameResultMutex in
      let lostConn = getTSValue lostConn lostConnMutex in
      let isGr = not (gro = None) in
      let isGr = isGr ||
        match lostConn with
          Some(c) ->
            performTSRefOp (fun gro -> gro := Some (Winner (c)))
              gameResult gameResultMutex; true
        | _ -> isGr
        in
      let _ = if isGr then
        match valOf gro with
          Tie -> print_endline "Game ended in a tie."
        | Winner (Red) -> print_endline "Red team wins!"
        | Winner (Blue) -> print_endline "Blue team wins!"
        else if sd then print_endline "Server shutting down..." else () in
      if not (sd || isGr) then (Thread.delay 0.5; waitForGameOver()) else
        (print_endline "killing connections."; Connection.close p1;
         Connection.close p2) in
    waitForGameOver();
    if getTSValue shutdown shutdownMutex then () else startNewGame() in

  (* this thread should also shut down on its own now that the sd signal is set appropriately *)
  let _ = Thread.create startNewGame () in

  let rec checkInput () =
    let i = read_line () in
    if i = "q" then
      let _ = print_endline "Shutting down server..." in
      performTSRefOp (fun s -> s := true) shutdown shutdownMutex
    else checkInput () in
  let _ = Thread.create checkInput () in

  while not (getTSValue shutdown shutdownMutex) do
    Thread.delay 0.1
  done;

  Mutex.lock playersMutex;
  List.iter Connection.close !players;
  Mutex.unlock playersMutex

let () =
  let port = try int_of_string ((Sys.argv).(1))
             with _ -> cDEFAULT_PORT_NUMBER in
  let guiPort = try int_of_string ((Sys.argv).(2))
                with _ -> cDEFAULT_GUI_PORT_NUMBER in
  startGameServer port guiPort
