(** $ ocamlfind ocamlopt -package cairo2 -linkpkg \
  *     src/dimacs.cmx src/jr_cairo.ml -o jr_cairo
  * $ ./jr s <n> | ./jr_cairo
  *
  * Format attendu:
  * ---------
  * <taille>
  * <ligne 1>
  * ...
  * <ligne N>
  * ---------
  * chaque ligne contenant N caractères dans ABCDEFGHIJK,
  * représentant les tuiles dans l'ordre de l'image.
  *
  * Exemple en taille 10:
10
BHGBIIIIIJ
EEEEDDDDDE
FFFFGCKGKF
GKGCBJAHGA
HGBJBHGBHG
EEEEEEEEEE
FFFFFFFFFF
CKGCKGKGCA
JCBJAHGBJA
BJBHGBIIHG
  *)


type piece = { n : int ; e : int ; s : int ; w : int }
let p = Array.map (fun (w,n,e,s) -> {w;n;e;s}) [|
  (3,1,1,1);
  (3,2,1,2);
  (3,1,3,3);
  (2,0,2,1);
  (2,2,2,0);
  (0,0,0,1);
  (0,1,3,2);
  (1,2,0,2);
  (1,2,1,0);
  (1,3,3,2);
  (3,1,0,1)
|]

let () =

  let n = int_of_string (read_line ()) in

  Format.printf "Reading tiling of size %d...\n%!" n ;

  let tile = Array.make_matrix n n p.(0) in
  for j = 0 to n-1 do
    let line = read_line () in
      for i = 0 to n-1 do
        tile.(i).(j) <- p.(int_of_char line.[i] - int_of_char 'A')
      done
  done ;

  Format.printf "Done reading lines.\n%!" ;

  for j = 0 to n-1 do
    for i = 0 to n-1 do
      if not (i=n-1 || tile.(i).(j).e = tile.(i+1).(j).w) then
        Format.printf "Failure to east of %d,%d!\n" i j ;
      if not (j=n-1 || tile.(i).(j).s = tile.(i).(j+1).n) then
        Format.printf "Failure to south of %d,%d!\n" i j
    done
  done ;

  Format.printf "Tiling is valid!\n"
