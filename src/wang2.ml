type 'a tile = {
    north: 'a;
    south: 'a;
    east: 'a;
    west: 'a;
}
let mktile n s e w = { north=n; south=s; east=e; west=w; }

let tileset = [|
    mktile 1 1 1 3;
    mktile 2 2 1 3;
    mktile 1 3 3 3;
    mktile 0 1 2 2;
    mktile 2 0 2 2;
    mktile 0 1 0 0;
    mktile 1 2 3 0;
    mktile 2 2 0 1;
    mktile 2 0 1 1;
    mktile 3 2 3 1;
    mktile 1 1 0 3
|]

let tsize = Array.length tileset

let fixpattern = [10; 6; 8; 3]
let use_forbidden_pattern = true

let colors = [| 0; 31; 34; 32; 33 |]

let print_color n =
    Format.printf "\x1b[%dm█\x1b[0m" colors.(n)

let domain n =
    let vertical = Dimacs.(make ((n+1) ** n ** 2 ** o)) in
    let horizontal = Dimacs.(make (n ** (n+1) ** 2 ** o)) in
    Array.init n (fun i ->
        Array.init n (fun j ->
            mktile
                vertical.(i).(j)
                vertical.(i+1).(j)
                horizontal.(i).(j+1)
                horizontal.(i).(j)
        )
    )

let problem size =
    let grid = domain size in
    for a = 0 to size-2 do
        for b = 0 to size-1 do
            assert (grid.(a).(b).south = grid.(a+1).(b).north);
            assert (grid.(b).(a).east = grid.(b).(a+1).west);
        done
    done;
    (* each position exactly one color -> already encoded in binary *)
    (* color compatibility -> already encoded by equality of variables *)
    (* each tile must be valid *)
    let apply b variable =
        if b = 1 then Dimacs.not variable else variable
    in
    for n = 0 to 3 do
        let (n0, n1) = (n mod 2, n / 2) in
        for s = 0 to 3 do
            let (s0, s1) = (s mod 2, s / 2) in
            for e = 0 to 3 do
                let (e0, e1) = (e mod 2, e / 2) in
                for w = 0 to 3 do
                    let (w0, w1) = (w mod 2, w / 2) in
                    if not (Array.mem (mktile n s e w) tileset) then (
                        (* This tile is not allowed, add one clause
                           (for each position) to express it *) 
                        for i = 0 to size-1 do
                            for j = 0 to size-1 do
                                let tile = grid.(i).(j) in
                                Dimacs.(add_clause [
                                    apply n0 tile.north.(0);
                                    apply n1 tile.north.(1);
                                    apply s0 tile.south.(0);
                                    apply s1 tile.south.(1);
                                    apply w0 tile.west.(0);
                                    apply w1 tile.west.(1);
                                    apply e0 tile.east.(0);
                                    apply e1 tile.east.(1)
                                ])
                            done
                        done
                    )
                done
            done
        done
    done
    (* insert forbidden pattern *)



let solution n =
    let grid = domain n in
    let m = Dimacs.read_model (open_in "output.sat") in
    let read_bin arr =
        match Dimacs.((sat m arr.(0), sat m arr.(1))) with
            | (false, false) -> 0
            | (true, false) -> 1
            | (false, true) -> 2
            | (true, true) -> 3
    in
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            Format.printf "┌";
            let k = read_bin grid.(i).(j).north in
            print_color k;
            Format.printf "┐"
        done;
        Format.printf "\n";
        for j = 0 to n-1 do
            let k = read_bin grid.(i).(j).west in
            print_color k;
            if
                n / 2 - 1 <= i
                && i <= n / 2 + 2
                && j = n / 2
            then print_color 4
            else Format.printf " ";
            let k = read_bin grid.(i).(j).east in
            print_color k
        done;
        Format.printf "\n";
        for j = 0 to n-1 do
            Format.printf "└";
            let k = read_bin grid.(i).(j).south in
            print_color k;
            Format.printf "┘"
        done;
        Format.printf "\n"
    done


let () = Dimacs.run_int ~problem ~solution


