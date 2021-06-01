(* Many hashtbl accesses to come, might as well make them easy *)
let ( .!{} ) = Hashtbl.find
let ( .!{}<- ) = Hashtbl.add
let ( .?{} ) = Hashtbl.mem

(* Mostly for debugging purposes *)
let inspect fn lst =
    List.iter fn lst;
    lst

type pos = int * int
type litarr = Dimacs.literal array array
type dom = {
    visit: litarr; (* sat if penguin is currently there *)
    free: litarr; (* sat if penguin has been there at some point *)
}
type env = {
    nb: int; (* number of positions *)
    to_num: (pos, int) Hashtbl.t;
    to_pos: (int, pos) Hashtbl.t;
}

let domain grid waste =
    let pos_to_num = Hashtbl.create 100 in
    let num_to_pos = Hashtbl.create 100 in
    let nb = ref 0 in
    Array.iteri (fun i line ->
        Array.iteri (fun j b ->
            if b then (
                (* two-way translation between ids and coordinates *)
                pos_to_num.!{i,j} <- !nb;
                num_to_pos.!{!nb} <- (i,j);
                incr nb
            )
        ) line
    ) grid;
    let nb = !nb in
    let turns = nb - waste in
    Format.printf "positions: %d, turns: %d\n" nb turns;
    let visit = Dimacs.(make (turns ** nb ** o)) in
    let free = Dimacs.(make (turns ** nb ** o)) in
    (
        {
            visit=visit;
            free=free;
        },
        {
            nb=nb;
            to_num=pos_to_num;
            to_pos=num_to_pos;
        }
    )

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
    let (dom, env) = domain grid waste in
    let turns = env.nb - waste in 
    (* [visit.(i).(k)] means that position [k] is explored at turn [i] *)
    let init () =
        Format.printf "has to start at the right position\n";
        Dimacs.(add_clause [dom.visit.(0).(env.to_num.!{start})]);
        for i = 0 to env.nb - 1 do
            if i <> env.to_num.!{start} then (
                Dimacs.(add_clause [not dom.visit.(0).(i)]);
                Dimacs.(add_clause [dom.free.(0).(i)])
            );
        done;
    in
    let accessibility =
        List.init env.nb (fun k ->
            let pos = env.to_pos.!{k} in
            Hex.all_directions
            |> List.map (accessible (fun p -> env.to_num.?{p}) pos)
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
            let k = env.to_num.!{start} in
            let reach = near
                |> List.map snd
            in
            for i = 1 to turns - 1 do
                (* if on [k] at [i-1] then has to be on a neighbor at [i] *)
                Dimacs.(add_clause (
                    not dom.visit.(i-1).(k)
                    :: List.map (fun n -> dom.visit.(i).(env.to_num.!{n})) reach
                ));
                (* if on [k] at [i-1], then all non-neighbors can't be reached at [i] *)
                for j = 0 to env.nb - 1 do
                    if j <> k && not (List.mem env.to_pos.!{j} reach) then (
                        Dimacs.(add_clause [
                            not dom.visit.(i-1).(k);
                            not dom.visit.(i).(j)
                        ])
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
                            not dom.visit.(i-1).(env.to_num.!{start});
                            not dom.visit.(i).(env.to_num.!{dest});
                            dom.free.(i-1).(env.to_num.!{inter})
                        ])
                    done
                );
            )
        )
    in
    let melt () =
        Format.printf "never explore the same position twice\n";
        for k = 0 to env.nb - 1 do
            for i = 0 to turns - 2 do
                Dimacs.(add_clause [ (* free not visited stays free *)
                    not dom.free.(i).(k);
                    dom.visit.(i+1).(k);
                    dom.free.(i+1).(k);
                ]);
            done
        done;
        for k = 0 to env.nb - 1 do
            for i = 0 to turns - 2 do
                Dimacs.(add_clause [ (* visited position becomes not free *)
                    not dom.visit.(i).(k);
                    not dom.free.(i).(k)
                ]);
                Dimacs.(add_clause [ (* not free stays not free *)
                    dom.free.(i).(k);
                    not dom.free.(i+1).(k);
                ]);
            done
        done
    in
    let unique () =
        Format.printf "can't be in two places at once\n";
        for i = 0 to turns - 1 do
            for k = 0 to env.nb - 1 do
                for k' = k + 1 to env.nb - 1 do
                    Dimacs.(add_clause [
                        not dom.visit.(i).(k);
                        not dom.visit.(i).(k')
                    ])
                done
            done
        done
    in
    let somewhere () =
        Format.printf "has to be somewhere";
        for i = 0 to turns-1 do
            Dimacs.(add_clause (bigor env.nb (fun k -> dom.visit.(i).(k))))
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
    let (dom, env) = domain grid waste in
    let turns = env.nb - waste in
    let m = Dimacs.read_model (open_in "tests/output.sat") in
    let nb_visited = ref 0 in
    let path = Array.mapi (fun i line ->
        Array.mapi (fun j b ->
            let idx = if b then (
                let k = env.to_num.!{i,j} in
                let turn = ref (-1) in
                for t = 0 to turns - 1 do
                    if Dimacs.sat m dom.visit.(t).(k) then (
                        turn := t;
                        incr nb_visited
                    )
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
    assert (!nb_visited = turns);
    Hex.pp_char_grid Format.std_formatter path
            
let () = Dimacs.run ~problem ~solution
