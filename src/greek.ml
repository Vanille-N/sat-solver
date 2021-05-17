let domain n =
    (* For each column i, row j and symbol k,
     * we have one variable x_i,j,k indicating that symbol k is there. *)
    let lat = Dimacs.(make (n**n**n**o)) in
    let grk = Dimacs.(make (n**n**n**o)) in
    (lat, grk)

let problem n =
    let (lat, grk) = domain n in
    (* For all i,j no more than one symbol at i,j for both lat and grk. *)
    (* also, no more than one [line|column] for each symbol *)
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            for k = 0 to n-1 do
                for k' = k+1 to n-1 do
                    Dimacs.(add_clause [not lat.(i).(j).(k); not lat.(i).(j).(k')]) ;
                    Dimacs.(add_clause [not grk.(i).(j).(k); not grk.(i).(j).(k')])
                done ;
                for i' = i+1 to n-1 do
                    Dimacs.(add_clause [not lat.(i).(j).(k); not lat.(i').(j).(k)]) ;
                    Dimacs.(add_clause [not grk.(i).(j).(k); not grk.(i').(j).(k)])
                done;
                for j' = j+1 to n-1 do
                    Dimacs.(add_clause [not lat.(i).(j).(k); not lat.(i).(j').(k)]) ;
                    Dimacs.(add_clause [not grk.(i).(j).(k); not grk.(i).(j').(k)])
                done
            done
        done
    done ;
    (* For all i,j there is a k such that x_i,j,k.
     * For all i,k there is a k such that x_i,j,k.
     * For all j,k there is a k such that x_i,j,k. *)
    for a = 0 to n-1 do
        for b = 0 to n-1 do
            Dimacs.(add_clause (bigor n (fun c -> lat.(a).(b).(c)))) ;
            Dimacs.(add_clause (bigor n (fun c -> lat.(a).(c).(b)))) ;
            Dimacs.(add_clause (bigor n (fun c -> lat.(c).(a).(b)))) ;
            Dimacs.(add_clause (bigor n (fun c -> grk.(a).(b).(c)))) ;
            Dimacs.(add_clause (bigor n (fun c -> grk.(a).(c).(b)))) ;
            Dimacs.(add_clause (bigor n (fun c -> grk.(c).(a).(b))))
        done
    done;
    (* now express the fact that there is no duplicate pairing *)
    for i = 0 to n-1 do
        for i' = 0 to n-1 do
            for j = 0 to n-1 do
                for j' = 0 to n-1 do
                    if i < i' || (i = i' && j < j') then (
                        for k = 0 to n-1 do
                            for k' = 0 to n-1 do
                                Dimacs.(add_clause [
                                    not lat.(i).(j).(k);
                                    not lat.(i').(j').(k);
                                    not grk.(i).(j).(k');
                                    not grk.(i').(j').(k')
                                ])
                            done
                        done
                    )
                done
            done
        done
    done;
    (* remove symmetry *)
    for i = 0 to n-1 do   (* 15s -> 200ms *)
        Dimacs.(add_clause [lat.(0).(i).(i)]);
        Dimacs.(add_clause [grk.(0).(i).(i)]);
        (*for j = 1 to n-1 do   (* 200ms -> 160ms *)
            Dimacs.(add_clause [not lat.(j).(i).(i)]);
            Dimacs.(add_clause [not grk.(j).(i).(i)]);
            (*if j <> i then (   (* 160ms -> 150ms *)
                  Dimacs.(add_clause [not lat.(0).(i).(j)]);
                  Dimacs.(add_clause [not lat.(0).(j).(i)]);
                  Dimacs.(add_clause [not grk.(0).(i).(j)]);
                  Dimacs.(add_clause [not grk.(0).(j).(i)]);
            )*)
        done*)
    done

let solution n =
    let (lat, grk) = domain n in
    let m = Dimacs.read_model (open_in "tests/output.sat") in
    for i = 0 to n-1 do
        for j = 0 to n-1 do
            Format.printf "[" ;
            for k = 0 to n-1 do
                if Dimacs.sat m lat.(i).(j).(k) then (
                    Format.printf "%c"
                        (char_of_int (k + int_of_char 'A'))
                )
            done ;
            for k = 0 to n-1 do
                if Dimacs.sat m grk.(i).(j).(k) then (
                    Format.printf "%c"
                        (char_of_int (k + int_of_char 'a'))
                )     
            done ;
            Format.printf "]"
        done ;
        Format.printf "\n"
    done

let () = Dimacs.run_int ~problem ~solution
