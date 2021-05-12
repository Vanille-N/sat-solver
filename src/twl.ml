exception Conflict
exception SAT
exception Found of int
exception Break

module Pair = struct
    type 'a t = {
        (* a type to represent an unordered pair *)
        mutable fst : 'a;
        mutable snd : 'a;
    }
    (* create a new pair *)
    let make a b = { fst = a; snd = b }

    let other p c =
        if p.fst = c then p.snd
        else (
            assert (p.snd = c); (* check consistency of pair *)
            p.fst
        )

    let replace p c c' =
        if p.fst = c then p.fst <- c'
        else (
            assert (p.snd = c); (* check consistency of pair *)
            p.snd <- c'
        )
end

type wclause = Model.literal Pair.t * Model.literal list (* (watched, clause) *)
type wclauses = (wclause, int) Dll.t (* int markers for rotation *)
type watch = wclauses array array
(* w.(i) = [| neg; pos |] where
   pos is the dll of clauses where  i is watched
   neg ''                       '' -i ''      ''
 *)

(* Sometimes there are duplicate literals in clauses
   This is the easiest solution that does not change the initial clause
   Type ['a list -> ('a * 'a, 'a) result]
   [Ok (a, a')] if two elements were found
   [Error a] if only one.
   Warning: fails when given an empty clause
   Warning: quadratic complexity in the [Error] case
 *)
let find_two_different lst =
    let rec find_different l = function
        | [] -> None
        | l'::_ when l <> l' -> Some l'
        | _::tl -> find_different l tl
    in
    let rec find_two = function
        | [] -> failwith "Empty clause"
        | l::tl -> match find_different l tl with
            | None -> find_two tl
            | Some l' -> Ok (l, l')
    in
    find_two lst

(* Marker to keep track of list rotations
   Incrementing the marker effectively resets all.
   This assumes that the algorithm executes fewer that 2^32 propagations,
   the alternative would be concerning anyway.
 *)
let marker = ref 1

(** One-step propagation. *)
let propagate_step m watch lit =
    let modified = ref [] in (* keep track of changes for future propagations *)
    let curr_watched = watch.(abs lit).(if lit > 0 then 0 else 1) in
    (* clauses in which one watched literal was just set to false *)
    incr marker; (* reset the rotation marker *)
    let rec take_all () = match Dll.peek curr_watched with
        | _ when Dll.get_mark curr_watched = Some !marker -> () (* full rotation done *)
        | None -> () (* no more watched clauses *)
        | Some (pair, cl) -> (
            Dll.set_mark curr_watched (Some !marker); (* keep track of rotation *)
            if List.exists (fun l -> Model.sat m l) cl
            then Dll.rotate curr_watched (* Nothing to do for this clause *)
            else (
                let single_unassigned l =
                    (* if there is only one unassigned literal, it must be set to true
                       The change must also be recorded to properly propagate
                     *)
                    assert (l = Pair.other pair (-lit));
                        (* If this is false the internal state of the structure
                           was corrupted: only one literal is not set but it is
                           not watched
                         *)
                    modified := l :: !modified;
                    Dll.rotate curr_watched;
                in
                let two_unassigned l l' =
                    (* Change watched to either one
                       If one is equal to the watched literal choose the other one,
                       otherwise it doesn't matter
                     *)
                    ignore (Dll.take curr_watched);
                    let other_w = Pair.other pair (-lit) in
                    let new_watch = if abs l = abs other_w then l' else l in
                    Pair.replace pair (-lit) new_watch;
                    Dll.insert
                        watch.(abs new_watch).(if new_watch > 0 then 1 else 0)
                        (pair, cl);
                    (* The combination
                       [Dll.take] + [Dll.insert]
                       implicitly performs a rotation
                     *)
                in
                match List.filter (fun l -> not (Model.assigned m l)) cl with
                    | [] -> raise Conflict
                    | [l] -> single_unassigned l
                    | more -> (
                        match find_two_different more with
                            | Error l -> single_unassigned l
                            | Ok (l,l') -> two_unassigned l l'
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
        match (Sys.getenv_opt "TRACE") with
            | None -> (fun _ -> ())
            | Some _ -> (
                let chan = Format.formatter_of_out_channel (open_out "trace.txt") in
                fun m -> Format.fprintf chan "%a@." Model.pp m
            )
    in
    (* Flag to show some debugging information.
     * You may add your own debugging information, but don't
     * mess with the traces as you will need them to be stable
     * over successive refinements of your implementation. *)
    let debug = (Sys.getenv_opt "DEBUG") <> None in
    (* Get clauses as lists of integers. *)
    let clauses = List.rev_map (List.map Dimacs.to_int) (Dimacs.get_clauses ()) in
    let rec propagate m watch = function
        | [] -> () (* fixpoint reached *)
        | l::_ when Model.sat m (-l) -> raise Conflict
        | l::rest -> (
            if not (Model.sat m l) then Model.add m l;
            (* check is necessary since
               - some previous propagation may have set l
               - Model does not accept duplicate affectation
             *)
            propagate m watch ((propagate_step m watch l) @ rest)
        )
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
        (* [choose] calculates the watched literals and
           the list of initial propagations *)
        let rec choose = function
            | [] -> []
            | clause::rest -> (
                match clause with
                    | [] -> failwith "Empty clause"
                    | [l] -> l :: (choose rest)
                    | lst -> (
                        match find_two_different lst with
                            | Error l -> l :: (choose rest)
                            | Ok (l, l') -> (
                                let p = Pair.make l l' in
                                (* watch [l] and [l'] in clause [lst] *)
                                Dll.insert
                                    watch.(abs l).(if l > 0 then 1 else 0)
                                    (p, clause);
                                Dll.insert
                                    watch.(abs l').(if l' > 0 then 1 else 0)
                                    (p, clause);
                                choose rest
                            )
                    )
            )
        in
        let units = choose clauses in
        (units, watch)
    in
    let dpll m =
        let (units, watch) = make_watch () in
        let rec aux ls =
            print_trace m;
            if debug then Format.printf "> %a@." Model.pp m;
            match propagate m watch ls with
                | exception Conflict -> ()
                | () -> (
                    let l = find_unassigned m in
                    (
                        Model.add m l;
                        aux [l];
                        Model.remove m l;
                    );
                    if debug then Format.printf "backtrack@." ;
                    (
                        Model.add m (-l);
                        aux [-l];
                        Model.remove m l
                    );
                )
        in aux units; (* initial propagations *)
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
