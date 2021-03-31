let () =

  let n = int_of_string (read_line ()) in

  Format.printf "Reading tiling of size %d...\n%!" n ;

  (* Graphics colors. *)
  let col = [| Graphics.rgb 255 0 0 ;
               Graphics.rgb 0 255 0 ;
               Graphics.rgb 0 0 255 |] in
  let color = Array.make_matrix n n col.(0) in

    (* Populate [color] using colors from [col]
     * based on description of tiling on standard input. *)
    for i = 0 to n-1 do
      let line = read_line () in
        for j = 0 to n-1 do
          color.(i).(j) <- col.(int_of_char line.[j] - int_of_char '0')
        done
    done ;

    Graphics.open_graph "" ;
    Graphics.resize_window 800 800 ;
    Graphics.set_line_width 7 ;

    for i = 0 to n-2 do
      for j = 0 to n-2 do
        Graphics.set_color color.(i).(j) ;
        Graphics.fill_circle (10+20*i) (10+20*j) 5 ;
        if color.(i+1).(j+1) = color.(i).(j) then begin
          Graphics.moveto (10+20*i) (10+20*j) ;
          Graphics.lineto (10+20*(i+1)) (10+20*(j+1))
        end else begin
          Graphics.set_color color.(i+1).(j) ;
          Graphics.moveto (10+20*(i+1)) (10+20*j) ;
          Graphics.lineto (10+20*i) (10+20*(j+1))
        end
      done
    done ;

    Graphics.(loop_at_exit [Key_pressed;Button_down] (fun _ -> raise Exit))
