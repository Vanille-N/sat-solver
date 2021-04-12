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
  * chaque ligne contenant N caractÃ¨res dans ABCDEFGHIJK,
  * reprÃ©sentant les tuiles dans l'ordre de l'image.
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

let filename = "tile.svg"

let () =

  let n = int_of_string (read_line ()) in
  Format.printf "Reading tiling of size %d...\n%!" n ;

  (* Cairo colors. *)
  let col = [| 1.,0.,0. ; 0.,0.5,0. ; 0.,0.,0. ; 0.,0.,1. |] in
  let tile = Array.make_matrix n n p.(0) in

  let () =
    (* Populate [color] using colors from [col]
     * based on description of tiling on standard input. *)
    for j = 0 to n-1 do
      let line = read_line () in
        for i = 0 to n-1 do
          tile.(i).(j) <- p.(int_of_char line.[i] - int_of_char 'A')
        done
    done ;
  in

  Format.printf "Done reading lines.\n%!" ;

  let size = 1000. in
  let s = Cairo.SVG.create filename size size in
  let c = Cairo.create s in
  (* let pi = acos (-1.) in *)

  let draw_straight i j =
    let tile = tile.(i).(j) in
    let i = float i in
    let j = float j in

    Cairo.set_line_width c 0.01 ; 
    Cairo.set_source_rgb c 0.5 0.5 0.5 ;
    Cairo.move_to c i j ;
    Cairo.line_to c (i+.1.) j ;
    Cairo.line_to c (i+.1.) (j+.1.) ;
    Cairo.line_to c i (j+.1.) ;
    Cairo.line_to c i j ;
    Cairo.stroke c ;

    Cairo.set_line_width c 0.2 ; 
    let draw ~sel ~x ~y ~full =
      let r,g,b = sel tile in
      Cairo.set_source_rgb c r g b ;
      Cairo.move_to c (i+.x) (j+.y) ;
      let x',y' =
        if full then i+.0.5,j+.0.5 else
         i+.(x+.0.5)/.2., j+.(y+.0.5)/.2. in
      Cairo.line_to c x' y' ;
      Cairo.stroke c
    in
    let params = [|
      (fun x -> col.(x.w)), 0., 0.5 ;
      (fun x -> col.(x.n)), 0.5, 0. ;
      (fun x -> col.(x.e)), 1., 0.5 ;
      (fun x -> col.(x.s)), 0.5, 1.
    |] in
      for i = 0 to 3 do
        let sel,x,y = params.(i) in
        let full =
          (let sel' = let s',_,_ = params.((i+2) mod 4) in s' in
             sel tile = sel' tile) ||
          (let sel' = let s',_,_ = params.((i+1) mod 4) in s' in
           let sel'' = let s',_,_ = params.((i+3) mod 4) in s' in
             sel tile = sel' tile && sel' tile = sel'' tile)
        in
          draw ~sel ~x ~y ~full
      done
  in

    Cairo.scale c 10. 10. ;
    Cairo.set_line_width c 0.1 ;
    Cairo.set_source_rgb c 0. 0. 0. ;
    Cairo.move_to c 0.5 0.5 ;
    Cairo.line_to c 2.0 0.5 ;
    Cairo.stroke c ;
    Cairo.move_to c 0.5 0.5 ;
    Cairo.line_to c 0.5 1.0 ;
    Cairo.stroke c ;
    Cairo.translate c 1. 1. ;
    Cairo.set_line_cap c Cairo.ROUND ;
    let draw =
      Cairo.set_line_width c 0.3 ; draw_straight
    in
    for j = 0 to n-1 do
      for i = 0 to n-1 do
        if not (i=n-1 || tile.(i).(j).e = tile.(i+1).(j).w) then
          Format.printf "Failure to east of %d,%d!\n" i j ;
        if not (j=n-1 || tile.(i).(j).s = tile.(i).(j+1).n) then
          Format.printf "Failure to south of %d,%d!\n" i j ;
        draw i j
      done
    done ;
    Cairo.Surface.finish s ;
    Format.printf "Tiling rendered in %S.\n" filename

