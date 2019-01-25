let read_file filename =
  let lines = ref "" in
  let chan = open_in filename in
  try
    while true; do
      lines := !lines^input_line chan
    done; !lines
  with End_of_file -> close_in chan; !lines;;


let _ =
  (* on définit l'entrée stadard comme buffer dans lequel lire les lexèmes *)
  let code = ref ((read_file Sys.argv.(1))) in
  let lexbuf = Lexing.from_string !code in

    (* on lance l'analyseur syntaxique avec l'analyseur lexical sur le buffer créé
    et on récupère l'abre de syntaxe abstraite p *)
  let p = Parser.main Lexer.token lexbuf

    (* on évalue p, puis on traduit le résultat en chaine de caractères et on l'affiche *)
  in
    if Eval.is_prog  p
    then begin
      print_endline "PROGRAMME : ";
      print_endline (Expr.toString p 1);
      ignore (Eval.eval p)
    end
    else begin
      print_endline(Expr.tostring p)
    end
