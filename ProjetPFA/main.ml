open Graphics
open Array
open String

(*
les cases 80 pixel de coté
pour centrer x+25


*)
let selectedCase = ref (-1,-1);;
let listeSudoku = ref [];;

module type Grille =
    sig
    type t
    val value : t
    val set : int -> int -> (char*bool) -> unit
    val get : int -> int -> char
    val isChangeable : int -> int -> bool
    end

module G : Grille = struct
    type t =  (char*bool) array array
    let value = Array.make_matrix 9 9 ('0',true)
    let set x y v = value.(x).(y) <- v
    let get x y = fst value.(x).(y) 
    let isChangeable x y = snd value.(x).(y) 
    end

let lire_fichier fichier tab =
    let cin = open_in fichier in
    try
        let line = input_line cin in
        close_in cin;
        for i = 0 to 8 do
            for j = 0 to 8 do
                let c = String.get line (9*i+j) in
                if Char.equal '0' c then tab.(i).(j) <- (c,true) else tab.(i).(j) <-(c,false);
            done 
        done
    with 
       |e-> close_in_noerr cin;raise e
;;

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


let afficheSudoku tab =
    trace_ligneGrille ();
    Graphics.set_font "-*-fixed-medium-r-semicondensed--75-*-*-*-*-*-iso8859-1";
    Graphics.set_color black;
    for i=0 to 8 do
        for j=0 to 8 do
            if not (Char.equal '0' (fst tab.(i).(j))) then begin Graphics.moveto (140+j*80+25) (690-i*80); Graphics.draw_char (fst tab.(i).(j)) end;
        done
    done ;;
    
exception NotInSudoku;;

let getCase x y =
    let checkLine y = 
    match y with
            |a when a <50 -> raise NotInSudoku
            |b when b > 770 -> raise NotInSudoku
            |c -> 8-((c-50)/80)(*80+50*)
    in
    let checkColumn x =
        match x with
            |a when a <140 -> raise NotInSudoku
            |b when b > 860 -> raise NotInSudoku
            |c -> ((c-140)/80)(*80+140*)
    in
    try
        (checkLine y,checkColumn x)
     with NotInSudoku -> (-1,-1)
    
;;

let highlightCase tab =
    let x = fst !selectedCase in
    let y = snd !selectedCase in
    if (x,y) <> (-1,-1) then 
        begin
        Graphics.set_color (Graphics.rgb 127 0 97);
        Graphics.fill_rect (y*80+140+1) ((8-x)*80+50+1) 78 78;
        end;
;;

let afficheJeu tab = 
    Graphics.clear_graph ();
    highlightCase tab;
    trace_ligneGrille ();
    afficheSudoku tab;
;;

let setSelectedCase x y tab =
    if snd (tab.(x).(y)) then selectedCase := (x,y) else selectedCase := (-1,-1)
;;

let retourEnArriereGrille tab =
    match !listeSudoku with
        |[] -> ()
        |h::t -> begin
         for i=0 to 8 do
            for j=0 to 8 do
                tab.(i).(j) <- h.(i).(j)
            done
         done ;
         listeSudoku := t
         end
;;

let setValueCase tab v =
    let x = fst !selectedCase in
    let y = snd !selectedCase in
    match v with 
       |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' when (x,y) <> (-1,-1)-> listeSudoku := tab::!listeSudoku;tab.(x).(y) <- (v,true)
       |'r' -> retourEnArriereGrille tab
       |_ -> ()
;;

let rec eventListener grille =
    let st = wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in 
    if st.keypressed then setValueCase grille st.key
    else 
        if st.button then 
            begin
            let a = getCase st.mouse_x st.mouse_y in
            setSelectedCase (fst a) (snd a) grille;
            end;
    
    afficheJeu grille;
    eventListener grille
;;
    

module S = G ;;
let test = Array.make_matrix 9 9 ('0',false);;
lire_fichier "grids/grid0.txt" test;;

Graphics.open_graph " 1000x1000";;
afficheJeu test;;
eventListener test;;


