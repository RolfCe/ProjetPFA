open Graphics
open Array
open String

module G : Grille = struct
    type 
    let value = Array.make_matrix 9 9 0
    let set x y v = value.(x).(y) <- v
    let get = value.(x).(y)
    end

let lire_fichier fichier tab =
    let cin = open_in fichier in
    try
        let line = input_line cin in
        for i = 0 to (String.length line) do
            let c = String.get line i in
            tab.(i) <- c;
            close_in cin
        done
    with 
       |e-> close_in_noerr cin;raise e
;;

Printf.printf "test \n";;

let trace_ligneGrille () = 
    Graphics.set_color black;
    Graphics.set_line_width 2;
    for i = 0 to 3 do
        Graphics.moveto (140+i*240) 50;
        Graphics.lineto (140+i*240) 770;
        Graphics.moveto 140 (50+240*i);
        Graphics.lineto (140+3*240) (50+240*i);
    done;
    
    Graphics.set_line_width 1;
    for i = 1 to 9 do
        Graphics.moveto (140+i*80) 50;
        Graphics.lineto (140+i*80) 770;
        Graphics.moveto 140 (50+80*i);
        Graphics.lineto (140+3*240) (50+80*i);
    done
;;


Graphics.open_graph " 1000x1000";;
trace_ligneGrille ();;
let st = wait_next_event [Button_down];;

Printf.printf "test \n";;
