(** Variable declarations *)

type literal = int

let not x = -x
let to_int x = x

let c = ref 0
let fresh () = incr c ; !c

let nb_variables () = !c

type _ index =
  | O : literal index
  | S : int * 'a index -> ('a array) index

let ( ** ) x y = S (x,y)
let o = O

let rec make : type a. a index -> a = function
  | O -> fresh ()
  | S (m,i) -> Array.init m (fun _ -> make i)

(** Clauses *)

type clause = literal list

let rec all acc n f =
  if n = 0 then acc else all (f (n-1) :: acc) (n-1) f

let bigor n f = all [] n f

let clauses = ref []

let add_clause l = clauses := l :: !clauses

let output chan =
  Printf.fprintf chan "p cnf %d %d\n" !c (List.length !clauses) ;
  List.iter
    (fun c ->
       List.iter (fun l -> Printf.fprintf chan "%d " l) c ;
       Printf.fprintf chan "0\n")
    !clauses

let get_clauses () = !clauses

(** Parsing *)

(** Convert a stream to a list, reversed. The stream must be finite. *)
let rec to_list acc stream =
  match Stream.next stream with
    | hd -> to_list (hd::acc) stream
    | exception Stream.Failure -> acc

(** [parse keywords chan] reads one line from [chan], parses it using
  * [Genlex] with the list of keywords [keywords], and returns the
  * resulting list of tokens as a list. *)
let rec parse_rev keywords chan =
  let lexer =
    Genlex.make_lexer ("c"::keywords) (Stream.of_string (input_line chan))
  in
  match Stream.next lexer with
    | Genlex.Kwd "c" -> parse_rev keywords chan
    | token -> to_list [token] lexer

let parse k c = List.rev (parse_rev k c)

exception Parse_error

let input chan =
  let nb_vars,nb_clauses =
    match parse ["p";"cnf"] chan with
      | [Genlex.Kwd "p"; Genlex.Kwd "cnf";
         Genlex.Int vars; Genlex.Int clauses] -> vars,clauses
      | _ -> raise Parse_error
  in
  let rec read_clause acc =
    match parse_rev [] chan with
      | Genlex.Int 0 :: l ->
          let l =
            List.rev_map
              (function Genlex.Int i -> i | _ -> raise Parse_error)
              l
          in
          let l = List.rev_append l acc in
            assert (l <> []) ;
            clauses := l :: !clauses
      | l ->
          let l =
            List.rev_map
              (function Genlex.Int i -> i | _ -> raise Parse_error)
              l
          in
            read_clause (List.rev_append l acc)
  in
    c := nb_vars ;
    begin try while true do read_clause [] done with End_of_file -> () end ;
    if List.length !clauses <> nb_clauses then begin
      Format.eprintf "Unexpected number of clauses!@." ;
      raise Parse_error
    end

(** Solution *)

type model = bool array

let read_model chan =

  (* First line must be SAT *)
  if input_line chan <> "SAT" then failwith "expected SAT" ;

  (* Second line is the model description, of the form <int>* 0. *)
  let line =
    List.map
      (function Genlex.Int i -> i | _ -> raise Parse_error)
      (parse [] chan)
  in
  let line = Array.of_list line in
    if Array.length line <> !c+1 || line.(!c) <> 0 then raise Parse_error ;
    Array.init (!c+1)
      (fun i ->
         if i=0 then false else
         let l = line.(i-1) in
         if l = i then true else
           if l = -i then false else
             failwith (Printf.sprintf "invalid input %d at %d" line.(i) i))

let sat m l = m.(l)

let usage () =
  Format.printf "Usage: %s <problem|solution> <descr>\n" Sys.argv.(0) ;
  exit 1

let run ~problem ~solution =
  if Array.length Sys.argv <> 3 then usage () ;
  if Sys.argv.(1) = "" then usage () ;
  if Sys.argv.(1).[0] = 'p' then begin
    problem Sys.argv.(2) ;
    output (open_out "problem.cnf")
  end ;
  if Sys.argv.(1).[0] = 's' then solution Sys.argv.(2)

let run_int ~problem ~solution =
  if Array.length Sys.argv <> 3 then usage () ;
  if Sys.argv.(1) = "" then usage () ;
  match int_of_string Sys.argv.(2) with
    | i ->
        if Sys.argv.(1).[0] = 'p' then begin
          problem i ;
          output (open_out "problem.cnf")
        end ;
        if Sys.argv.(1).[0] = 's' then solution i
    | exception Failure _ -> usage ()
