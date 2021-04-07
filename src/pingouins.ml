let domain grid =
    let nb = (
        let nb = ref 0 in
        Array.iter (Array.iter (function true -> incr nb | _ -> ())) grid;
        !nb
    ) in
    Format.printf "%d\n" nb;
    Dimacs.(make (nb**nb**o))

let problem file =
    Format.printf "Reading file %s\n" file;
    let (pos, grid) = Hex.from_channel (open_in file) in
    Hex.pp_bool_grid Format.std_formatter grid;
    ()



let solution file = failwith "Unimplemented"






let () = Dimacs.run ~problem ~solution
