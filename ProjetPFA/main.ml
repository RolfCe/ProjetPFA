open Graphics
open Array
open String
open Button
open Menu
open Random

(*
les cases 80 pixel de coté
pour centrer x+25


*)
let selectedCase = ref (-1,-1);;
let selectedNumberHighlight = ref '0';;
let listeSudoku = ref [];;
let lastMove = Array.make_matrix 9 9 ('0',false);; (* Juste pour initialiser *)
let countHelp = ref 5;; 
Random.self_init ();;
let seed = ref "-1";;

let afficheCountHelp  () =
    Graphics.moveto 2 780;
    Graphics.set_color black;
    Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
    Graphics.draw_string ( "Clue left : "^( string_of_int ( !countHelp ) ) );
   
;;
    
    
    
let dessinerBouton x y text taille gapW gapH width height =
    Graphics.moveto x y;
    Graphics.set_color black;
    Graphics.set_font ("-*-fixed-medium-r-semicondensed--"^taille^"-*-*-*-*-*-iso8859-1");
    Graphics.draw_string text;
    let lon = x - gapW in
    let la = y - gapH in
    
    Graphics.moveto lon la;
    Graphics.lineto ( lon + width ) la ; 
    Graphics.lineto ( lon + width )  (la + height ) ; 
    Graphics.lineto lon (la + height) ;
    Graphics.lineto lon la; 
;;

let actionHelp () =
    Graphics.moveto 75 950;
    Graphics.set_color black;
    Graphics.set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
    Graphics.draw_string "Key actions : [1-9]-Write 0-Erase s-Save l-Load r-BackTrack h-hint v-Check";
    Graphics.moveto 115 900;
    Graphics.draw_string "Rules : Each number can only appear once in a row, column or box";   
;;

module HelpButton : Button  =
    struct
    let x = 4
    let y = 970
    let width = 50
    let height = 25
    let drawButton () = dessinerBouton x y "Help" "25" 3 1 width height(* Déterminer par des moyens arbitraires*)
    let action () = actionHelp ()
    end;;

module Help = HelpButton ;;



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
    Graphics.set_line_width 3;
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
    Graphics.set_font "-*-fixed-medium-r-normal--75-*-*-*-*-*-iso8859-1";
    Graphics.set_color black;
    let grey = Graphics.rgb 105 105 105 in
    for i=0 to 8 do
        for j=0 to 8 do
            if not (Char.equal '0' (fst tab.(i).(j))) 
                then 
                    begin 
                        Graphics.moveto (140+j*80+25) (690-i*80); 
                        if snd tab.(i).(j) 
                            then 
                                begin Graphics.set_color grey; Graphics.draw_char (fst tab.(i).(j)) end 
                            else 
                                begin Graphics.set_color black;Graphics.draw_char (fst tab.(i).(j)) end
                    end;
        done
    done 
    ;;
    
exception NotInSudoku;;

let getCase x y =
    let checkLine y = 
    match y with
            |a when a <50 -> raise NotInSudoku
            |b when b > 770 -> raise NotInSudoku
            |c -> 8-((c-50)/80)
    in
    let checkColumn x =
        match x with
            |a when a <140 -> raise NotInSudoku
            |b when b > 860 -> raise NotInSudoku
            |c -> ((c-140)/80)
    in
    try
        (checkLine y,checkColumn x)
     with NotInSudoku -> (-1,-1)
    
;;

let highlightCase tab =
    let x = fst !selectedCase in
    let y = snd !selectedCase in
    let yi = (8-x)*80+50+2 in   
    let xi = y*80+140+2 in
    if (x,y) <> (-1,-1) then 
        begin
        Graphics.set_color (Graphics.rgb 0 0 0);
        Graphics.set_line_width 5;
        Graphics.moveto xi yi;
        Graphics.lineto (xi+76) yi;
        Graphics.lineto (xi+76) (yi+76);
        Graphics.lineto xi (yi+76);
        Graphics.lineto xi yi;
        end;
;;

let highlightCase_v1 tab x y =
    if (x,y) <> (-1,-1) then 
        begin
        Graphics.set_color (Graphics.rgb 127 0 97);
        Graphics.fill_rect (y*80+140+1) ((8-x)*80+50+1) 78 78;
        end;
;;

exception NoNumberSelected;;

let highlightAllNumber tab =
    let x = fst !selectedCase in
    let y = snd !selectedCase in
    if (x,y) = (-1,-1) then () else
    begin
    let n = fst tab.(x).(y) in
        if n = '0' 
            then 
                ()
            else 
                begin
                for i = 0 to 8 do
                    for j = 0 to 8 do
                        if (fst tab.(i).(j)) = n then highlightCase_v1 tab i j ;
                    done
                done
                end
    end
;;

let afficheJeu tab = 
    highlightAllNumber tab;
    highlightCase tab;
    Help.drawButton ();
    afficheCountHelp (); 
    trace_ligneGrille ();
    afficheSudoku tab;

;;

let setSelectedCase x y tab =
    if snd (tab.(x).(y)) then selectedCase := (x,y) else selectedCase := (-1,-1)
;;



let retourEnArriereGrille tab =
    match !listeSudoku with
        |[] -> ()
        |h::t -> 
         begin
         for i=0 to 8 do
            for j=0 to 8 do
                tab.(i).(j) <- h.(i).(j)
            done
         done ;
         listeSudoku := t
         end
;;

let checkGrille grilleAnswer grilleSoluce =
    let rec checkCase x y grilleAnswer grilleSoluce =
        let answer = fst grilleAnswer.(x).(y) in
        let soluce = fst grilleSoluce.(x).(y) in
        let test = (answer = soluce) in
        match x,y with
            |8,8 -> if answer = '0' then true else test
            |_,8 -> if answer = '0' || test then checkCase (x+1) 0 grilleAnswer grilleSoluce else false
            |_,_ -> if answer = '0' || test then checkCase x (y+1) grilleAnswer grilleSoluce else false
    in 
    checkCase 0 0 grilleAnswer grilleSoluce
;;

let checkSudokuIsComplete tab =
    let rec isComplete tab x y =
        let value = fst tab.(x).(y) in
        match x,y with
            |_,_ when value = '0' -> false
            |8,8 -> true
            |_,8 -> isComplete tab (x+1) 0
            |_,_ -> isComplete tab x (y+1)
    in
    isComplete tab 0 0
;;

let checkGameWinOrLost tab aIndex =
    let tabAnswer = Array.make_matrix 9 9 ('0',false) in
    lire_fichier ("solutions/solution"^aIndex^".txt") tabAnswer;
    Graphics.clear_graph ();
     if checkGrille tab tabAnswer
        then
            begin
            Graphics.set_font "-*-fixed-medium-r-semicondensed--100-*-*-*-*-*-iso8859-1";
            Graphics.set_color green;
            Graphics.moveto 250 500;
            Graphics.draw_string "GAME WIN";
            let st = wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in ()
            end
        else
            begin
            Graphics.set_font "-*-fixed-medium-r-semicondensed--100-*-*-*-*-*-iso8859-1";
            Graphics.set_color red;
            Graphics.moveto 250 500;
            Graphics.draw_string "GAME LOST";
            let st = wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in ()
            end
;;


let checkAnswerCurrent tab = (* Par rapport à la grille en entière  *)
    let tabAnswer = Array.make_matrix 9 9 ('0',false) in
    lire_fichier ("solutions/solution"^(!seed)^".txt") tabAnswer;
    if checkGrille tab tabAnswer 
    then
        begin
        Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
        Graphics.set_color green;
        Graphics.moveto 200 780;
        Graphics.draw_string "All Correct Answer(s) so far"
        end
    else
        begin
        Graphics.set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
        Graphics.set_color red;
        Graphics.moveto 200 780;
        Graphics.draw_string "There are Incorrect Answer(s)" 
        end
;;

let helpPlayer_addValue tab =
    let tabSoluce = Array.make_matrix 9 9 ('0',false) in
    lire_fichier ("solutions/solution"^(!seed)^".txt") tabSoluce;
    let rec addValue x y tab tabSoluce =
        let answer = fst tab.(x).(y) in
        let soluce = fst tabSoluce.(x).(y) in
        match x,y with
            |_,_ when answer = '0' -> listeSudoku := tab::!listeSudoku;tab.(x).(y) <- (soluce,true)
            |8,8 -> ()
            |_,8 -> addValue (x+1) 0 tab tabSoluce
            |_,_ -> addValue x (y+1) tab tabSoluce
    in
    addValue 0 0 tab tabSoluce  
;;

let giveClue_random tab =
    let tabSoluce = Array.make_matrix 9 9 ('0',false) in
    lire_fichier ("solutions/solution"^(!seed)^".txt") tabSoluce;
;;

let saveSudoku grille  =
    let save = open_out "save/save.txt" in
    for i = 0 to 8 do
        for j = 0 to 8 do
            Printf.fprintf save "%c" (fst grille.(i).(j));
        done;
    done;
    Printf.fprintf save "\n%s" !seed;
    close_out save
;;

let loadSudoku tab = 
    try
        let cSave = open_in "save/save.txt" in
        let lineSave = input_line cSave in
        seed := input_line cSave;
        let cin = open_in ("grids/grid"^(!seed)^".txt") in
        let line = input_line cin in
        close_in cSave;
        close_in cin;
        for i = 0 to 8 do
            for j = 0 to 8 do
                let c = String.get line (9*i+j) in
                let cSave = String.get lineSave (9*i+j) in
                if Char.equal '0' c then tab.(i).(j) <- (cSave,true) else tab.(i).(j) <-(c,false);
            done;
        done

    with
        |e-> raise e
;;

let saveLastMove tab =
        for i = 0 to 8 do
            for j = 0 to 8 do
                lastMove.(i).(j)<-tab.(i).(j) 
            done;
        done
;;

let loadLastMove tab =
        for i = 0 to 8 do
            for j = 0 to 8 do
                tab.(i).(j)<-lastMove.(i).(j) 
            done;
        done    
;;

let setValueCase tab v =
    let x = fst !selectedCase in
    let y = snd !selectedCase in
    match v with 
       |'0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' when (x,y) <> (-1,-1)-> saveLastMove tab;tab.(x).(y) <- (v,true)
       |'r' -> loadLastMove tab
       |'v'-> checkAnswerCurrent tab
       |'h' -> if !countHelp > 0 then begin  countHelp := !countHelp -1 ;  helpPlayer_addValue tab end 
       |'s' -> saveSudoku tab
       |'l' -> loadSudoku tab
       |_ -> ()
;;

let rec eventListener grille =
    let st = wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in 
    Graphics.clear_graph ();
    if st.keypressed then setValueCase grille st.key
    else 
        if st.button then 
            begin
            let a = getCase st.mouse_x st.mouse_y in
            match a with
                |(-1,-1)-> if (st.mouse_x>= Help.x) && (st.mouse_x<=Help.x+Help.width) && (st.mouse_y>=Help.y) && (st.mouse_y<= Help.y+Help.height) then Help.action ();
                |_ -> setSelectedCase (fst a) (snd a) grille;
            end;
    afficheJeu grille;
    if checkSudokuIsComplete grille then checkGameWinOrLost grille "0" else eventListener grille
;;
    
let main () =
    seed := string_of_int (Random.int 244);
    Graphics.resize_window 1000 1000;            (*Si la résolution de l'écran est trop petite, on ne verra pas tous les éléments du jeu*)
    let grille = Array.make_matrix 9 9 ('0',false) in
    lire_fichier ("grids/grid"^(!seed)^".txt") grille;
    lire_fichier ("grids/grid"^(!seed)^".txt") lastMove;
    afficheJeu grille;
    eventListener grille

;;

let loadGame () =
    Graphics.resize_window 1000 1000; 
    let grille = Array.make_matrix 9 9 ('0',false) in
    loadSudoku grille;
    loadSudoku lastMove;
    Graphics.open_graph " 1000x1000";
    afficheJeu grille;
    eventListener grille

;;

module NewGameButton : Button =
    struct
    let x = 425
    let y = 300
    let width = 275
    let height = 100
    let action () = main ()
    let drawButton () = dessinerBouton x y "START !!" "50" 50 25 width height
    end;;

module LoadButton : Button =
    struct
    let x = 412
    let y = 175
    let width = 300
    let height = 100
    let action () = loadGame ()
    let drawButton () = dessinerBouton x y "LOAD SAVE" "50" 50 25 width height
    end;;

module NewGame = NewGameButton ;;
module Load = LoadButton ;;

let rec menuEventlistener () = let st = wait_next_event [Graphics.Button_down] in 
        if st.button then 
            begin
            if (st.mouse_x >= Load.x) && (st.mouse_x <= Load.x + Load.width) && (st.mouse_y >= Load.y) && (st.mouse_y <= Load.y + Load.height) then Load.action ()
            else if (st.mouse_x >= NewGame.x) && (st.mouse_x <= NewGame.x + NewGame.width) && (st.mouse_y >= NewGame.y) && (st.mouse_y <= NewGame.y + NewGame.height)          
                    then   NewGame.action ()
            else menuEventlistener ()
            end
            
let drawTitle () = 
	Graphics.set_color black;
	Graphics.set_font "-*-fixed-medium-r-semicondensed--150-*-*-*-*-*-iso8859-1";
	Graphics.moveto 200 500;
	Graphics.draw_string "EL SUDOKU"
;;

module MenuStart : Menu =
    struct
    let eventListenerBouton () =  menuEventlistener () 
    let affichage () = Graphics.open_graph " 1000x700";set_window_title "EL SUDOKU";drawTitle ();NewGame.drawButton (); Load.drawButton () 
    end;;

module ElMenu = MenuStart;;

ElMenu.affichage ();;
ElMenu.eventListenerBouton ();;

