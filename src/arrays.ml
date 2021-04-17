(** Variables (i.e. propositional constants) are represented by positive
  * integers. Literals are arbitrary integers: for any variable X coded
  * as a positive integer [n], the positive literal X is represented by
  * [n] and not(X) is represented by [-n].
  *
  * A partial assignment [m] is an association list from variables
  * (i.e. positive literals) to booleans. *)

type literal = int

module type MODEL = sig
    type m
    (* create a new model *)
    val make : int -> m
    (* test if a literal is satisfied (unassigned is not satisfied) *)
    val sat : m -> literal -> bool
    (* test if a literal is assigned *)
    val assigned : m -> literal -> bool
    (* assign value to literal *)
    val add : m -> literal -> unit
    (* unassign all hypothesis made after a certain literal was set *)
    val remove : m -> literal -> unit
    (* pretty-print *)
    val pp : Format.formatter -> m -> unit
end

module ModelArrOpt : MODEL = struct
    type m = {
        assign : bool option array;
        mutable trace : int list;
    }

    let make size = {
        assign = Array.make (size+1) None;
        trace = [];
    }
    
    let sat model lit =
        match model.assign.(abs lit) with
            | None -> false
            | Some b -> if lit > 0 then b else not b

    let assigned model lit =
        model.assign.(abs lit) <> None
    
    let add model lit =
        let idx = abs lit in
        assert (model.assign.(idx) = None);
        model.trace <- idx :: model.trace;
        model.assign.(idx) <- Some (lit > 0)

    let remove model lit =
        let idx = abs lit in
        let rec aux = function
            | [] -> failwith "Trace should not be empty"
            | l :: rest ->
                model.assign.(l) <- None;
                if l = idx
                then model.trace <- rest
                else aux rest
        in aux model.trace

    let pp chan m =
        let l = m.assign
            |> Array.to_list
            |> List.mapi (fun i o -> (i, o))
            |> List.filter_map (fun (i,o) -> Option.map (fun b -> (i,b)) o)
        in
        Format.fprintf chan "[#%d:" (List.length l);
        List.iter (fun (i,b) -> Format.fprintf chan " %d" (if b then i else -i)) l;
        Format.fprintf chan "]"
end

module ModelTwoArr : MODEL = struct
    type m = {
        value : bool array;
        assigned : bool array;
        mutable trace : int list;
    }

    let make size = {
        value = Array.make (size+1) false;
        assigned = Array.make (size+1) false;
        trace = [];
    }

    let sat model lit =
        let idx = abs lit in
        model.assigned.(idx) && (if lit > 0 then model.value.(idx) else not model.value.(idx))

    let assigned model lit =
        model.assigned.(abs lit)

    let add model lit =
        let idx = abs lit in
        assert (model.assigned.(idx) = false);
        model.value.(idx) <- (lit > 0);
        model.assigned.(idx) <- true;
        model.trace <- idx :: model.trace

    let remove model lit =
        let idx = abs lit in
        let rec aux = function
            | [] -> failwith "Trace should not be empty"
            | l :: rest ->
                model.assigned.(l) <- false;
                if l = idx
                then model.trace <- rest
                else aux rest
        in aux model.trace

    let pp chan m =
        let l = m.value
            |> Array.to_list
            |> List.mapi (fun i v -> (i, v))
            |> List.filter (fun (i,_) -> m.assigned.(i))
        in
        Format.fprintf chan "[#%d:" (List.length l);
        List.iter (fun (i,b) -> Format.fprintf chan " %d" (if b then i else -i)) l;
        Format.fprintf chan "]"
end

module Model = ModelTwoArr

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
