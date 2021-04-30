(** Variables (i.e. propositional constants) are represented by positive
  * integers. Literals are arbitrary integers: for any variable X coded
  * as a positive integer [n], the positive literal X is represented by
  * [n] and not(X) is represented by [-n].
  *
  * A partial assignment [m] is an association list from variables
  * (i.e. positive literals) to booleans. *)

exception Conflict
exception SAT
exception Found of int
exception Break


type wclause = Model.literal * Model.literal list
type wclauses = (wclause, int) Dll.t
type watch = wclauses array array
(* w.(i) = [| neg; pos |] where
   pos is the dll of clauses where  i is watched
   neg ''                       '' -i ''      ''
 *)

 let marker = ref 1

 failwith "Other watched literal is not properly updated because in a different list"

(** One-step propagation. *)
let propagate_step m clauses watch lit =
    let modified = ref [] in
    let curr_watched = watch.(abs lit).(if lit > 0 then 0 else 1) in
    (* clauses in which one watched literal was just set to false *)
    incr marker;
    let rec take_all () = match Dll.peek curr_watched with
        | _ when Dll.get_mark curr_watched = Some !marker -> ()
        | None -> ()
        | Some (other_w, cl) -> (
            Dll.set_mark curr_watched (Some !marker);
            if List.exists (fun l -> Model.sat m l) cl then ()
            else (
                match List.filter (fun l -> not (Model.assigned m l)) cl with
                    | [] -> raise Conflict
                    | [l] -> (
                        if (l <> other_w) then failwith (Format.sprintf "%d <> %d\n" l other_w);
                        Model.add m l;
                        modified := l :: !modified; (* must be true *)
                        Dll.rotate curr_watched;
                    )
                    | l::l'::_ -> (
                        (* change watched to either one *)
                        ignore (Dll.take curr_watched);
                        let new_watch = if abs l = abs other_w then l' else l in
                        Dll.insert watch.(abs new_watch).(if new_watch > 0 then 1 else 0) (other_w, cl);
                    )
            );
            take_all ()
        )
    in 
    take_all ();
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
    let rec propagate m watch = function
        | [] -> ()
        | l::rest when not (Model.sat m l) -> raise Conflict
        | l::rest -> propagate m watch (propagate_step m clauses watch l @ rest)
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
    let make_watch () =
        let watch = Array.init
            (Dimacs.nb_variables () + 1)
            (fun i -> [| Dll.make (); Dll.make () |])
        in
        let rec choose = function
            | [] -> ()
            | clause::rest -> (
                match clause with
                    | [] -> failwith "Empty clause"
                    | [l] -> failwith "Unit clause"
                    | l::l'::_ -> (
                        if l = l' then failwith "Duplicate literal"
                        else (
                            Dll.insert watch.(abs l).(if l > 0 then 1 else 0) (l', clause);
                            Dll.insert watch.(abs l').(if l' > 0 then 1 else 0) (l, clause);
                            choose rest
                        )
                    )
            )
        in
        choose clauses;
        watch
    in
    let dpll m =
        let watch = make_watch () in
        let rec aux ls =
            print_trace m;
            if debug then Format.printf "> %a@." Model.pp m;
            match propagate m watch ls with
                | exception Conflict -> ()
                | () -> (
                    let l = find_unassigned m in
                    Model.add m l;
                    aux [l];
                    Model.remove m l;
                    if debug then Format.printf "backtrack@." ;
                    Model.add m (-l);
                    aux [-l];
                    Model.remove m l
                )
        in aux [];
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
