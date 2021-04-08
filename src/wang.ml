type tile = {
    north: int;
    south: int;
    east: int;
    west: int;
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
    Dimacs.(make (n ** n ** tsize ** o))

let problem n =
    let grid = domain n in
    (* each positions holds no more than one tile *)
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            for k = 0 to tsize-1 do
                for k' = k+1 to tsize-1 do
                    Dimacs.(add_clause [not grid.(i).(j).(k); not grid.(i).(j).(k')])
                done
            done
        done
    done;
    (* each position holds one tile *)
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            Dimacs.(add_clause (bigor tsize (fun k -> grid.(i).(j).(k))))
        done
    done;
    (* express adjacency constraints *)
    for k = 0 to tsize-1 do
        for k' = 0 to tsize-1 do
            if tileset.(k).south <> tileset.(k').north then (
                for i = 0 to n-2 do
                    for j = 0 to n-1 do
                        Dimacs.(add_clause [not grid.(i).(j).(k); not grid.(i+1).(j).(k')])
                    done
                done;
            );
            if tileset.(k).east <> tileset.(k').west then ( 
                for i = 0 to n-1 do
                    for j = 0 to n-2 do
                        Dimacs.(add_clause [not grid.(i).(j).(k); not grid.(i).(j+1).(k')])
                    done
                done
            )
        done
    done;
    (* better to express them in another way *)
    for k = 0 to tsize-1 do
        let t = tileset.(k) in
        let tiles = List.init tsize (fun i -> i) in
        let candidates fn = List.filter fn tiles in
        let right_adj = candidates (fun k' -> tileset.(k').west = t.east) in
        let left_adj = candidates (fun k' -> tileset.(k').east = t.west) in
        let up_adj = candidates (fun k' -> tileset.(k').south = t.north) in
        let down_adj = candidates (fun k' -> tileset.(k').north = t.south) in
        for i = 0 to tsize-1 do
            for j = 0 to tsize-2 do
                Dimacs.(add_clause (not grid.(i).(j).(k) :: List.map (fun k' -> grid.(i).(j+1).(k')) right_adj))
            done
        done;
        for i = 0 to tsize-2 do
            for j = 0 to tsize-1 do
                Dimacs.(add_clause (not grid.(i).(j).(k) :: List.map (fun k' -> grid.(i+1).(j).(k')) down_adj))
            done
        done;
        for i = 0 to tsize-1 do
            for j = 1 to tsize-1 do
                Dimacs.(add_clause (not grid.(i).(j).(k) :: List.map (fun k' -> grid.(i).(j-1).(k')) left_adj))
            done
        done;
        for i = 1 to tsize-1 do
            for j = 0 to tsize-1 do
                Dimacs.(add_clause (not grid.(i).(j).(k) :: List.map (fun k' -> grid.(i-1).(j).(k')) up_adj))
            done
        done
    done;
    (* insert forbidden pattern *)
    if use_forbidden_pattern then (
        let j0 = n / 2 in
        let i0 = n / 2 - 1 in
        List.iteri (fun di k ->
            Dimacs.(add_clause [grid.(i0+di).(j0).(k)]);
        ) fixpattern
    )



let solution n =
    let grid = domain n in
    let m = Dimacs.read_model (open_in "output.sat") in
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            Format.printf "┌";
            for k = 0 to tsize-1 do
                if Dimacs.sat m grid.(i).(j).(k)
                then print_color tileset.(k).north
            done;
            Format.printf "┐"
        done;
        Format.printf "\n";
        for j = 0 to n-1 do
            for k = 0 to tsize-1 do
                if Dimacs.sat m grid.(i).(j).(k)
                then print_color tileset.(k).west
            done;
            if
                n / 2 - 1 <= i
                && i <= n / 2 + 2
                && j = n / 2
            then print_color 4
            else Format.printf " ";
            for k = 0 to tsize-1 do
                if Dimacs.sat m grid.(i).(j).(k)
                then print_color tileset.(k).east
            done
        done;
        Format.printf "\n";
        for j = 0 to n-1 do
            Format.printf "└";
            for k = 0 to tsize-1 do
                if Dimacs.sat m grid.(i).(j).(k)
                then print_color tileset.(k).south
            done;
            Format.printf "┘"
        done;
        Format.printf "\n"
    done


let () = Dimacs.run_int ~problem ~solution


