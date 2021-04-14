(** Variables (i.e. propositional constants) are represented by positive
  * integers. Literals are arbitrary integers: for any variable X coded
  * as a positive integer [n], the positive literal X is represented by
  * [n] and not(X) is represented by [-n].
  *
  * A partial assignment [m] is an association list from variables
  * (i.e. positive literals) to booleans. *)

exception Conflict
exception SAT of (int*bool) list
exception Found of int
exception Break

(** [sat m l] indicates whether a literal is satisfied by a partial
  * assignment. Satisfaction is always false for unassigned literals. *)
let sat m l =
  if l > 0 then List.mem (l,true) m else List.mem (-l,false) m

(** Return new partial assignment where a previously unassigned literal
  * is set to true. *)
let add m l =
  if l > 0 then (l,true)::m else (-l,false)::m

(** Indicates whether a litteral is assigned in a partial assignment. *)
let assigned m l =
  if l > 0 then List.mem_assoc l m else List.mem_assoc (-l) m

(** One-step propagation. *)
let propagate_step m clauses =
  List.fold_left
    (fun m c ->
       if List.exists (fun l -> sat m l) c then m else
         match List.filter (fun l -> not (sat m (-l))) c with
           | [] -> raise Conflict
           | [l] -> add m l
           | l::_ -> m)
    m
    clauses

(** Pretty-print a model. *)
let pp_model chan l =
  Format.fprintf chan "[#%d:" (List.length l) ;
  List.iter
    (fun (i,b) -> Format.fprintf chan " %d" (if b then i else -i))
    l ;
  Format.fprintf chan "]"

(** Run DPLL for current Dimacs problem. *)
let dpll out =

  (* Function for producing the trace file, if TRACE is enabled.
   * Don't mess with the traces as you will need them to be stable
   * over successive refinements of your implementation. *)
  let print_trace =
    try
      ignore (Sys.getenv "TRACE") ;
      let chan = Format.formatter_of_out_channel (open_out "trace.txt") in
      fun m -> Format.fprintf chan "%a@." pp_model m
    with _ -> fun _ -> ()
  in

  (* Flag to show some debugging information.
   * You may add your own debugging information, but don't
   * mess with the traces as you will need them to be stable
   * over successive refinements of your implementation. *)
  let debug =
    try ignore (Sys.getenv "DEBUG") ; true with Not_found -> false in

  (* Get clauses as lists of integers. *)
  let clauses =
    List.rev_map (List.map Dimacs.to_int) (Dimacs.get_clauses ()) in

  let rec propagate m =
    let m' = propagate_step m clauses in
      if m = m' then m else propagate m'
  in

  let rec find_unassigned m =
    try
      List.iter
        (fun c ->
           let unassigned = ref 0 in
           try
             List.iter
               (fun l ->
                  if sat m l then raise Break ;
                  if not (assigned m l) then unassigned := l)
               c ;
             assert (!unassigned <> 0) ;
             raise (Found !unassigned)
           with Break -> ())
        clauses ;
      (* If we exit [List.iter] then all clauses are SAT. *)
      raise (SAT m)
    with Found i -> i
  in


  (* DPLL algorithm. *)
  let rec dpll m =
    print_trace m ;
    if debug then Format.printf "> %a@." pp_model m ;
    match propagate m with
      | exception Conflict -> ()
      | m ->
          let l = find_unassigned m in
            dpll (add m l) ;
            if debug then Format.printf "backtrack@." ;
            dpll (add m (-l))
  in
    try
      dpll [] ;
      Format.printf "UNSAT\n" ;
      Printf.fprintf out "UNSAT\n"
    with SAT m ->
      Format.printf "SAT\n" ;
      Printf.fprintf out "SAT\n" ;
      for i = 1 to Dimacs.nb_variables () do
        Printf.fprintf out "%d "
          (if sat m i then i else -i)
      done ;
      Printf.fprintf out "0\n"

let () =
  if Array.length Sys.argv <> 3 then begin
    Format.printf "Usage: %s <input.cnf> <output.model>\n" Sys.argv.(0) ;
    exit 1
  end ;
  let input = open_in Sys.argv.(1) in
  let output = open_out Sys.argv.(2) in
  Dimacs.input input ;
  dpll output
