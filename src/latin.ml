let domain n =
  (* For each column i, row j and symbol k,
   * we have one variable x_i,j,k indicating that symbol k is there. *)
  Dimacs.(make (n**n**n**o))

let problem n =
  let x = domain n in
  (* For all i,j no more than one symbol at i,j. *)
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      for k = 0 to n-1 do
        for k' = k+1 to n-1 do
          Dimacs.(add_clause [not x.(i).(j).(k); not x.(i).(j).(k')])
        done
      done
    done
  done ;
  (* For all i,j there is a k such that x_i,j,k.
   * For all i,k there is a k such that x_i,j,k.
   * For all j,k there is a k such that x_i,j,k. *)
  for a = 0 to n-1 do
    for b = 0 to n-1 do
      Dimacs.(add_clause (bigor n (fun c -> x.(a).(b).(c)))) ;
      Dimacs.(add_clause (bigor n (fun c -> x.(a).(c).(b)))) ;
      Dimacs.(add_clause (bigor n (fun c -> x.(c).(a).(b))))
    done
  done

let solution n =
  let x = domain n in
  let m = Dimacs.read_model (open_in "output.sat") in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      for k = 0 to n-1 do
        if Dimacs.sat m x.(i).(j).(k) then
          Format.printf "%c"
            (char_of_int
               (k + int_of_char 'A'))
      done
    done ;
    Format.printf "\n"
  done

let () = Dimacs.run_int ~problem ~solution
