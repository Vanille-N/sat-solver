let ( .!{} ) = Hashtbl.find
let ( .!{}<- ) = Hashtbl.add
let ( .?{} ) = Hashtbl.mem

let inspect fn lst =
    List.iter fn lst;
    lst

let domain grid waste =
    let pos_to_num = Hashtbl.create 100 in
    let num_to_pos = Hashtbl.create 100 in
    let nb = ref 0 in
    Array.iteri (fun i line ->
        Array.iteri (fun j b ->
            if b then (
                pos_to_num.!{i,j} <- !nb;
                num_to_pos.!{!nb} <- (i,j);
                incr nb
            )
        ) line
    ) grid;
    let nb = !nb in
    Format.printf "%d\n" nb;
    (nb, pos_to_num, num_to_pos, Dimacs.(make ((nb-waste) ** nb ** o)))

let waste = int_of_string (Sys.getenv "PENALTY")

let accessible existing center dir =
    let rec aux acc p n =
        if existing p then
            (p::acc, p) :: aux (p::acc) Hex.(move p dir) (n+1)
        else []
    in
    aux [] Hex.(move center dir) 0


let problem file =
    Format.printf "Reading file %s with penalty %d\n" file waste;
    let (start, grid) = Hex.from_channel (open_in file) in
    Hex.pp_bool_grid Format.std_formatter grid;
    let (nb, pos_to_num, num_to_pos, visit) = domain grid waste in
    let turns = nb - waste in 
    (* [visit.(i).(k)] means that position [k] is explored at turn [i] *)
    let init () =
        Format.printf "has to start at the right position\n"
    in
    Dimacs.(add_clause [visit.(0).(pos_to_num.!{start})]);
    let accessibility =
        List.init nb (fun k ->
            let pos = num_to_pos.!{k} in
            Hex.all_directions
            |> List.map (accessible (fun p -> pos_to_num.?{p}) pos)
            |> List.flatten
            (*|> inspect (fun (intermediate, ending) ->
                    Format.printf "(%d,%d) is neighbor of (%d,%d) [%d intermediate]\n"
                    (fst ending) (snd ending)
                    (fst pos) (snd pos)
                    (List.length intermediate)
            )*)
            |> fun lst -> (pos, lst)
        )
    in
    let compatible () =
        Format.printf "next position has to be accessible\n";
        accessibility
        |> List.iter (fun (start, near) ->
            let k = pos_to_num.!{start} in
            let reach = near
                |> List.map snd
            in
            (*Format.printf "neg (%d,%d) or any (%s)\n" (fst start) (snd start) (reach |> List.map (fun (i,j) -> Format.sprintf "(%d,%d)" i j) |> String.concat "; ");*)
            for i = 1 to turns-1 do
                (* if on [k] at [i] then has to be on a neighbor at [i+1] *)
                Dimacs.(add_clause (not visit.(i-1).(k) :: List.map (fun n -> visit.(i).(pos_to_num.!{n})) reach));
                (* if on [k] at [i], then all non-neighbors can't be reached at [i+1] *)
                for j = 0 to nb-1 do
                    if j <> k && not (List.mem num_to_pos.!{j} reach) then (
                        Dimacs.(add_clause [not visit.(i-1).(k); not visit.(i).(j)])
                    )
                done
            done
        );
    in
    let glide () =
        Format.printf "intermediate positions can't be already explored\n";
        accessibility
        |> List.iter (fun (start, near) ->
            near
            |> List.iter (fun (intermediate, dest) ->
                (* if turn [i] is [start -> dest] then none of [intermediate] may be previously explored *)
                for i = 1 to turns-1 do
                    for j = 0 to i-2 do
                        intermediate
                        |> List.iter (fun inter ->
                            Dimacs.(add_clause [
                                not visit.(i-1).(pos_to_num.!{start});
                                not visit.(i).(pos_to_num.!{dest});
                                not visit.(j).(pos_to_num.!{inter})
                            ])
                        );
                    done;
                done;
            )
        )
    in
    let melt () =
        Format.printf "never explore the same position twice\n";
        for k = 0 to nb-1 do
            let p = num_to_pos.!{k} in
            for i = 1 to turns-1 do
                for j = i+1 to turns-1 do
                    Dimacs.(add_clause [
                        not visit.(i).(pos_to_num.!{p});
                        not visit.(j).(pos_to_num.!{p})
                    ])
                done
            done
        done
    in
    let unique () =
        Format.printf "you can't be in two places at once\n";
        for i = 0 to turns-1 do
            Dimacs.(add_clause (bigor nb (fun k -> not visit.(i).(k))));
        done
    in
    init ();
    compatible ();
    unique ();
    glide ();
    melt ();
    ()



let solution file = failwith "Unimplemented"






let () = Dimacs.run ~problem ~solution
