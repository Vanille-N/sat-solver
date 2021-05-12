(* NOTE: All code relevant to arrays is implemented
   in the module Model.
   This file is only a copy of naive.ml that calls
   Model.foo instead of foo
 *)

exception Conflict
exception SAT
exception Found of int
exception Break

(** One-step propagation. *)
let propagate_step m clauses =
    let modified = ref false in
    List.iter (fun c ->
        if List.exists (fun l -> Model.sat m l) c
        then ()
        else match List.filter (fun l -> not (Model.sat m (-l))) c with
            | [] -> raise Conflict
            | [l] -> (Model.add m l; modified := true)
            | l::_ -> ()
    ) clauses;
    !modified

(** Run DPLL for current Dimacs problem. *)
let dpll out =
    (* Function for producing the trace file, if TRACE is enabled.
     * Don't mess with the traces as you will need them to be stable
     * over successive refinements of your implementation. *)
    let print_trace =
        try
            ignore (Sys.getenv "TRACE") ;
            let chan = Format.formatter_of_out_channel (open_out "trace.txt") in
            fun m -> Format.fprintf chan "%a@." Model.pp m
        with _ -> fun _ -> ()
    in
    (* Flag to show some debugging information.
     * You may add your own debugging information, but don't
     * mess with the traces as you will need them to be stable
     * over successive refinements of your implementation. *)
    let debug =
        try ignore (Sys.getenv "DEBUG"); true
        with Not_found -> false
    in
    (* Get clauses as lists of integers. *)
    let clauses =
        List.rev_map (List.map Dimacs.to_int) (Dimacs.get_clauses ())
    in
    let rec propagate m =
        let changed = propagate_step m clauses in
        if changed then propagate m else ()
    in
    let rec find_unassigned m =
        try
            List.iter (fun c ->
                let unassigned = ref 0 in
                try
                    List.iter (fun l ->
                        if Model.sat m l then raise Break ;
                        if not (Model.assigned m l) then unassigned := l
                    ) c;
                    assert (!unassigned <> 0);
                    raise (Found !unassigned)
                with Break -> ()
            ) clauses;
            (* If we exit [List.iter] then all clauses are SAT. *)
            raise SAT
        with Found i -> i
    in
    (* DPLL algorithm. *)
    let dpll m =
        let rec aux () =
            print_trace m;
            if debug then Format.printf "> %a@." Model.pp m;
            match propagate m with
                | exception Conflict -> ()
                | () -> (
                    let l = find_unassigned m in
                    Model.add m l;
                    aux ();
                    Model.remove m l;
                    if debug then Format.printf "backtrack@." ;
                    Model.add m (-l);
                    aux ();
                    Model.remove m l
                )
        in aux ()
    in
    let m = Model.make (Dimacs.nb_variables ()) in
    try
        dpll m;
        Format.printf "UNSAT\n";
        Printf.fprintf out "UNSAT\n"
    with SAT -> (
        Format.printf "SAT\n" ;
        Printf.fprintf out "SAT\n" ;
        for i = 1 to Dimacs.nb_variables () do
            Printf.fprintf out "%d "
                (if Model.sat m i then i else -i)
        done;
        Printf.fprintf out "0\n"
    )

let () =
    if Array.length Sys.argv <> 3 then (
        Format.printf "Usage: %s <input.cnf> <output.model>\n" Sys.argv.(0);
        exit 1
    );
    let input = open_in Sys.argv.(1) in
    let output = open_out Sys.argv.(2) in
    Dimacs.input input;
    dpll output
