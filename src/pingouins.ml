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
    let turns = nb - waste in
    let visit = Dimacs.(make (turns ** nb ** o)) in
    let free = Dimacs.(make (turns ** nb ** o)) in
    (nb, pos_to_num, num_to_pos, visit, free)

let waste = match Sys.getenv_opt "PENALTY" with
    | Some s -> int_of_string s
    | None -> failwith "PENALTY not found"

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
    let (nb, pos_to_num, num_to_pos, visit, free) = domain grid waste in
    let turns = nb - waste in 
    (* [visit.(i).(k)] means that position [k] is explored at turn [i] *)
    let init () =
        Format.printf "has to start at the right position\n";
        Dimacs.(add_clause [visit.(0).(pos_to_num.!{start})]);
        for i = 0 to nb-1 do
            if i <> pos_to_num.!{start} then (
                Dimacs.(add_clause [not visit.(0).(i)]);
                Dimacs.(add_clause [free.(0).(i)])
            );
        done;
    in
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
            for i = 1 to turns-1 do
                (* if on [k] at [i-1] then has to be on a neighbor at [i] *)
                Dimacs.(add_clause (not visit.(i-1).(k) :: List.map (fun n -> visit.(i).(pos_to_num.!{n})) reach));
                (* if on [k] at [i-1], then all non-neighbors can't be reached at [i] *)
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
                intermediate
                |> List.iter (fun inter ->
                    for i = 1 to turns-1 do
                        Dimacs.(add_clause [
                            not visit.(i-1).(pos_to_num.!{start});
                            not visit.(i).(pos_to_num.!{dest});
                            free.(i-1).(pos_to_num.!{inter})
                        ])
                    done
                );
            )
        )
    in
    let melt () =
        Format.printf "never explore the same position twice\n";
        for k = 0 to nb-1 do
            let p = num_to_pos.!{k} in
            for i = 1 to turns-2 do
                Dimacs.(add_clause [
                    not visit.(i).(pos_to_num.!{p});
                    not free.(i).(pos_to_num.!{p})
                ]);
                Dimacs.(add_clause [
                    free.(i).(pos_to_num.!{p});
                    not free.(i+1).(pos_to_num.!{p});
                ]);
            done
        done
    in
    let unique () =
        Format.printf "can't be in two places at once\n";
        for i = 0 to turns-1 do
            for k = 0 to nb-1 do
                for k' = k+1 to nb-1 do
                    Dimacs.(add_clause [not visit.(i).(k); not visit.(i).(k')])
                done
            done
        done
    in
    let somewhere () =
        Format.printf "has to be somewhere";
        for i = 0 to turns-1 do
            Dimacs.(add_clause (bigor nb (fun k -> visit.(i).(k))))
        done
    in
    init ();
    (* somewhere (); *)
    unique ();
    compatible ();
    glide ();
    melt ();
    ()

let solution file =
    let (start, grid) = Hex.from_channel (open_in file) in
    let (nb, pos_to_num, num_to_pos, visit, _) = domain grid waste in
    let turns = nb - waste in
    let m = Dimacs.read_model (open_in "output.sat") in
    let path = Array.mapi (fun i line ->
        Array.mapi (fun j b ->
            let idx = if b then (
                let k = pos_to_num.!{i,j} in
                let turn = ref (-1) in
                for t = 0 to turns-1 do
                    if Dimacs.sat m visit.(t).(k) then turn := t
                done;
                !turn
            ) else -2 in
            match idx with
                | -2 -> ' '
                | -1 -> '*'
                | n when 0 <= n && n < 26 -> char_of_int (int_of_char 'a' + n)
                | n when 26 <= n && n < 26*2 -> char_of_int (int_of_char 'A' + n - 26)
                | _ -> '?'
        ) line
    ) grid in
    Hex.pp_char_grid Format.std_formatter path
            
let () = Dimacs.run ~problem ~solution
