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

module Pair = struct
    type 'a t = {
        (* a type to represent an unordered pair *)
        mutable fst : 'a;
        mutable snd : 'a;
    }
    let make a b = { fst = a; snd = b }

    let other p c =
        if p.fst = c then p.snd
        else p.fst

    let replace p c c' =
        if p.fst = c then p.fst <- c'
        else (assert (p.snd = c); p.snd <- c')
end

type wclause = Model.literal Pair.t * Model.literal list
type wclauses = (wclause, int) Dll.t
type watch = wclauses array array
(* w.(i) = [| neg; pos |] where
   pos is the dll of clauses where  i is watched
   neg ''                       '' -i ''      ''
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

let marker = ref 1

(** One-step propagation. *)
let propagate_step m watch lit =
    (* Format.printf "Propagating %d\n" lit; *)
    let modified = ref [] in
    let curr_watched = watch.(abs lit).(if lit > 0 then 0 else 1) in
    (* clauses in which one watched literal was just set to false *)
    incr marker;
    let rec take_all () = match Dll.peek curr_watched with
        | _ when Dll.get_mark curr_watched = Some !marker -> ()
        | None -> ()
        | Some ((pair:Model.literal Pair.t), cl) -> (
            Dll.set_mark curr_watched (Some !marker);
            (* Format.printf "<"; List.iter (Format.printf " %d ") cl; Format.printf ">\n"; *)
            (* Format.printf "watchers: %d %d\n" pair.fst pair.snd; *)
            if List.exists (fun l -> Model.sat m l) cl then Dll.rotate curr_watched
            else (
                match List.filter (fun l -> not (Model.assigned m l)) cl with
                    | [] -> raise Conflict
                    | [l] -> (
                        if (l <> Pair.other pair (-lit)) then failwith (Format.sprintf "%d <> %d\n" l (Pair.other pair (-lit)));
                        (* Format.printf "           Add %d\n" l; *)
                        modified := l :: !modified; (* must be true because its other watched literal was just set to false and there is now just one literal in the clause *)
                        Dll.rotate curr_watched;
                    )
                    | l::l'::_ -> (
                        assert (l <> l');
                        (* change watched to either one *)
                        ignore (Dll.take curr_watched);
                        let other_w = Pair.other pair (-lit) in
                        let new_watch = if abs l = abs other_w then l' else l in
                        Pair.replace pair (-lit) new_watch;
                        (* Format.printf "Changing watch from %d to %d\n" (-lit) new_watch; *)
                        (* Format.printf " -> %d and %d\n" pair.fst pair.snd; *)
                        Dll.insert watch.(abs new_watch).(if new_watch > 0 then 1 else 0) (pair, cl);
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
        | l::rest when Model.sat m (-l) -> raise Conflict
        | l::rest -> (
            if not (Model.sat m l) then Model.add m l;
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
                                Dll.insert watch.(abs l).(if l > 0 then 1 else 0) (p, clause);
                                Dll.insert watch.(abs l').(if l' > 0 then 1 else 0) (p, clause);
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
                    Model.add m l;
                    aux [l];
                    Model.remove m l;
                    if debug then Format.printf "backtrack@." ;
                    Model.add m (-l);
                    aux [-l];
                    Model.remove m l
                )
        in aux units;
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
