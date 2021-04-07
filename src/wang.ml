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

let colors = [| 31; 32; 33; 34 |]

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

let () = Dimacs.run_int ~problem ~solution


