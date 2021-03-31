
(** Positions et grilles *)

type pos = int*int

type 'a grid = 'a array array

(** Affichage d'une grille *)

let pp_char_grid chan grid =
  let width = Array.length grid in
  let height = Array.length grid.(0) in
    for i = 0 to width - 1 do
      if i mod 2 = 0 then Format.pp_print_char chan ' ' ;
      for j = 0 to height - 1 do
        Format.pp_print_char chan grid.(i).(j) ;
        Format.pp_print_char chan ' '
      done ;
      Format.pp_print_char chan '\n'
    done

let pp_bool_grid chan grid =
  pp_char_grid chan
    (Array.map
       (Array.map
          (function true -> '.' | _ -> ' '))
       grid)

let pp_solution chan grid path =
  let grid =
    Array.map
      (Array.map
         (function true -> '.' | _ -> ' '))
      grid
  in
  let char_of_int p =
    if p>=62 then '!' else
      if p<26 then char_of_int (97+p) else (* a..z *)
        if p<52 then char_of_int (39+p) else (* A..Z *)
          char_of_int (p-4) (* 0..9 *)
  in
    Array.iteri
      (fun i (x,y) ->
         grid.(x).(y) <- char_of_int i)
      path ;
    pp_char_grid chan grid

(** Lecture d'une grille *)

let blank_line line =
  try
    for i = 0 to String.length line - 1 do
      if line.[i] <> ' ' then raise Exit
    done ;
    true
  with Exit -> false

let rec from_channel chan =

  while input_line chan <> "<problem>" do () done ;
  let rec get_lines () =
    let line = input_line chan in
      if line <> "</problem>" then ("  "^line^"  ") :: get_lines () else []
  in
  let rec remove_first_blank = function
    | line::lines ->
        if blank_line line then
          remove_first_blank lines
        else
          line::lines
    | [] -> failwith "empty hex graph"
  in
  let lines = remove_first_blank (get_lines ()) in
  let lines = List.rev (remove_first_blank (List.rev lines)) in

  let lines =
    let line = List.hd lines in
    let rec find i =
      if line.[i] = ' ' then find (i+1) else i
    in
    let i = find 0 in
      if i mod 2 = 1 then "" :: lines else lines
  in
  let lines = "" :: lines @ [""] in

  let width = List.length lines in
  let height =
    List.fold_left
      (fun m l -> max m (1 + String.length l / 2))
      0 lines
  in

  let lines = Array.of_list lines in
  let start,hex =
    let start = ref (-1,-1) in
    let hex = Array.make_matrix width height false in
      for i = 0 to width - 1 do
        for j = 0 to height - 1 do
          let jj = (1+i) mod 2 + j*2 in
            if jj < String.length lines.(i) then begin
              if lines.(i).[jj] <> ' ' then
                hex.(i).(j) <- true ;
              if lines.(i).[jj] = '#' then
                start := (i,j)
            end
        done
      done ;
      !start,hex
  in

    start, hex

(** Directions et mouvements *)

type dir = N | NE | SE | S | SW | NW

(** Print with a rotation to be consistent with ascii printing
  * of hex grids. *)
let string_of_dir = function
  | N -> "E"
  | S -> "W"
  | NE -> "SE"
  | SE -> "SW"
  | NW -> "NE"
  | SW -> "NW"

let all_directions = [N;NE;SE;S;SW;NW]

let move (x,y) = function
  | N -> x,y+1
  | S -> x,y-1
  | SE -> x+1, y - (x mod 2)
  | NW -> x-1, y + ((1+x) mod 2)
  | SW -> x-1, y - (x mod 2)
  | NE -> x+1, y + ((1+x) mod 2)

type move = dir*int

let rec move_n pos (dir,n) =
  if n = 0 then pos else move_n (move pos dir) (dir,n-1)

let path_of_moves start moves =
  let rec pom pos = function
    | move::moves ->
        let pos' = move_n pos move in
          pos :: pom pos' moves
    | [] -> [pos]
  in
    pom start moves
