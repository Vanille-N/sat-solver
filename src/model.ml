type literal = int

type m = {
    (* None -> unassigned; Some b -> assigned to b *)
    assign : bool option array;
    (* most recent changes first, all should be positive *)
    mutable trace : int list;
}

let make size = {
    (* all start unassigned *)
    assign = Array.make (size+1) None;
    trace = [];
}
    
let sat model lit =
    match model.assign.(abs lit) with
        | None -> false
        | Some b -> (lit > 0) = b (* i.e. n is assigned to true is b is true and -n is assigned to true if b is false *)

let assigned model lit =
    model.assign.(abs lit) <> None

let add model lit =
    let idx = abs lit in
    assert (model.assign.(idx) = None);
    model.trace <- idx :: model.trace;
    model.assign.(idx) <- Some (lit > 0)

let remove model lit =
    (* assignments are properly ordered -> cancelling all changes since assignment of lit
       is the same as unassigning all literals encountered before lit is seen in the trace *)
    let idx = abs lit in
    let rec aux = function
        | [] -> failwith "Trace should not be empty"
        | l :: rest ->
            model.assign.(l) <- None;
            if l = idx (* contents of the trace are positive *)
            then model.trace <- rest (* end of the backtrace *)
            else aux rest (* keep going *)
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