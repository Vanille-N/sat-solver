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
    Format.printf "Reading file %s\n" file;
    let (pos, grid) = Hex.from_channel (open_in file) in
    Hex.pp_bool_grid Format.std_formatter grid;
    ()



let solution file = failwith "Unimplemented"






let () = Dimacs.run ~problem ~solution
