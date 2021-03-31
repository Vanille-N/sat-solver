let filename = "outputs/tile.svg"

let () =

  let n = int_of_string (read_line ()) in
  Format.printf "Reading tiling of size %d...\n%!" n ;

  (* Cairo colors. *)
  let col = [| 1.,0.,0. ; 0.,0.,0. ; 1.,1.,1. |] in
  let color = Array.make_matrix n n col.(0) in

  let () =
    (* Populate [color] using colors from [col]
     * based on description of tiling on standard input. *)
    for i = 0 to n-1 do
      let line = read_line () in
        for j = 0 to n-1 do
          color.(i).(j) <- col.(int_of_char line.[j] - int_of_char '0')
        done
    done ;
  in

  let size = 1000. in
  let s = Cairo.SVG.create filename size size in
  let c = Cairo.create s in
  let pi = acos (-1.) in

  let draw_straight i j =
    let r,g,b = color.(i).(j) in
    Cairo.set_source_rgb c r g b ;
    Cairo.arc c (float i) (float j) ~r:0.3 ~a1:0. ~a2:(2.*.pi) ;
    Cairo.fill c ;
    Cairo.move_to c (float i) (float j) ;
    if color.(i+1).(j+1) = color.(i).(j) then begin
      Cairo.rel_line_to c 1. 1. ;
      Cairo.stroke c
    end else begin
      let r,g,b = color.(i+1).(j) in
      Cairo.set_source_rgb c r g b ;
      Cairo.rel_move_to c 1. 0. ;
      Cairo.rel_line_to c (-1.) 1. ;
      Cairo.stroke c
    end
  in
  let draw_round i j =

    let tile_diag = color.(i).(j) = color.(i+1).(j+1) in

    (* Draw background *)
    let r,g,b = if tile_diag then color.(i).(j) else color.(i+1).(j) in
    Cairo.set_source_rgb c r g b ;
    Cairo.move_to c (float i) (float j) ;
    Cairo.rel_line_to c 0. 1. ;
    Cairo.rel_line_to c 1. 0. ;
    Cairo.rel_line_to c 0. (-1.) ;
    Cairo.rel_line_to c (-1.) 0. ;
    Cairo.fill c ;
    (* Draw corners *)
    let corner i j a1 a2 =
      let x = float i and y = float j in
      Cairo.move_to c x y ;
      Cairo.arc c x y ~r:0.5 ~a1 ~a2 ;
      let r,g,b = color.(i).(j) in
      Cairo.set_source_rgb c r g b ;
      Cairo.fill c ;
      Cairo.set_source_rgb c 0. 0. 0. ;
      Cairo.arc c x y ~r:0.5 ~a1 ~a2 ;
      Cairo.stroke c
    in
    if not tile_diag then begin
      corner i j 0. (pi/.2.) ;
      corner (i+1) (j+1) pi (3.*.pi/.2.)
    end else begin
      corner (i+1) j (pi/.2.) pi ;
      corner i (j+1) (3.*.pi/.2.) (2.*.pi)
    end
  in

    Cairo.scale c 10. 10. ;
    Cairo.translate c 1. 1. ;
    Cairo.set_line_cap c Cairo.ROUND ;
    let draw =
      match Sys.getenv "STYLE" with
        | "straight" -> Cairo.set_line_width c 0.3 ; draw_straight
        | _ -> Cairo.set_line_width c 0.1 ; draw_round
        | exception Not_found -> Cairo.set_line_width c 0.1 ; draw_round
    in
    for i = 0 to n-2 do
      for j = 0 to n-2 do
        draw i j
      done
    done ;
    Cairo.Surface.finish s ;
    Format.printf "Tiling rendered in %S.\n" filename
