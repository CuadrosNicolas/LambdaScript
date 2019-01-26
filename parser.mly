%{ (* prelude en OCaml *)

  open Expr


  let remove_end str count =
    let n = String.length str in
    let n_name = String.sub str 0 (n-count) in
    n_name

  let toStr str =
    let n = String.length str in
    let n_name = String.sub str 1 (n-2) in
    n_name


  let rec listToFun l e =
    match l with
    | [] -> e
    | h::t -> Fun(h,listToFun t e)

  let rec tuplelen l =
    match l with
    | TupleNode(a,b) -> 1+
        (
          match b with
            | TupleNode(_,_) -> tuplelen b
            | _               -> 1
        )
    | _             -> 0

  let rec make_app e l =
    match l with
    | a :: [] -> App(e,a)
    | a :: r -> make_app (App (e,a)) r
    | []    -> failwith "Error : empty list"

 %}


/*déclaration des tokens */

%token <int> INT
%token <string> IDENT
%token <string> ATOM
%token <string> NAMEDFUN
%token <string> RECNAME
%token <string> STRING
%token <float> FLOAT
%token BEGIN_FUNCTION DEFINITION END_FUNCTION
%token EOF
%token PLUS SUB DIV MULT LPAREN RPAREN MODULO
%token TRUE FALSE
%token IF THEN ELSE END
%token EQ DIFF INF INFEQ SUP SUPEQ
%token AND OR NOT
%token REC
%token SEPARATOR
%token UNIFY MATCHEX WITH ANY ARROW PIPE
%token ARRAY_END ARRAY_LINK
%token SEQUENCER PRINT
%token TOINT
%token TOFLOAT
%token TOSTRING
%token TOBOOL
%token POSTCOND


/* gestion des priorités/associativités */
%right TOINT TOBOOL TOFLOAT TOSTRING
%right  SEPARATOR ARRAY_LINK SEQUENCER ATOM
%left EQ DIFF INF INFEQ SUP SUPEQ POSTCOND
%left AND OR
%left PLUS SUB
%left MULT DIVIDE MODULO ATOM
%right NOT



%nonassoc DEFINITION
%nonassoc END_FUNCTION
%start main               /*  entry point    */
%type <Expr.expr> main
%%
  main:
  declare EOF                              { $1                        }
  ;
  declare:
  | expr                                  {$1}
  ;
  expr:
  | simpleexpr                          { $1                        }
  | PRINT   expr             {Print $2}
  | TOINT   expr                  {ToInt $2}
  | TOFLOAT   expr                  {ToFloat $2}
  | TOBOOL   expr                  {ToBool $2}
  | TOSTRING   expr                  {ToString $2}
  | expr SEQUENCER expr      {Sequencer($1,$3)}
  | expr MODULO expr                   { Mod($1,$3)                }
  | expr PLUS expr                   { Add($1,$3)                }
  | expr SUB expr                   { Sub($1,$3)                }
  | SUB   expr                      {Neg($2)}
  | expr DIV expr                   { Div($1,$3)                }
  | expr MULT expr                   { Mult($1,$3)                }
  | expr EQ   expr                  {Eq($1,$3)}
  | expr INF   expr                  {Inf($1,$3)}
  | expr INFEQ   expr                  {Infeq($1,$3)}
  | expr SUP   expr                  {Sup($1,$3)}
  | expr SUPEQ   expr                  {Supeq($1,$3)}
  | expr DIFF expr                  {Diff($1,$3)}
  | expr AND expr                   {And($1,$3)}
  | expr OR expr                   {And($1,$3)}
  | NOT expr                   {Not $2}
  | simpleexpr simpleexprlist           { make_app $1 (List.rev $2) } /* application associative a gauche */

  | RECNAME IDENT DEFINITION expr END_FUNCTION expr      {RecFun((remove_end $1 2),$2,$4,$6)}
  | RECNAME IDENT  separatelist DEFINITION expr END_FUNCTION expr      {RecFun (remove_end $1 2,$2,(listToFun (List.rev $3) $5),$7)}

  | NAMEDFUN IDENT DEFINITION expr END_FUNCTION expr      {NamedFun(remove_end $1 1,$2,$4,$6)}
  | NAMEDFUN IDENT  separatelist DEFINITION expr END_FUNCTION expr      {NamedFun(remove_end $1 1,$2,(listToFun (List.rev $3) $5),$7)}

  | NAMEDFUN expr END_FUNCTION expr      {NamedExp(remove_end $1 1,$2,$4)}

  | IF expr THEN expr ELSE expr  END                         {Cond ($2,$4,$6)}
  | ATOM LPAREN expr tuplenode RPAREN            {NamedTuple($1,Tuple(tuplelen (TupleNode($3,$4)),TupleNode($3,$4)))}
  | ATOM LPAREN expr RPAREN            {NamedTuple($1,Tuple(1,TupleNode($3,TupleEnd)))}
  | ATOM LPAREN RPAREN                        {Atom $1}
  | expr tuplenode                               {Tuple(tuplelen (TupleNode($1,$2)),TupleNode($1,$2))}
  ;

  simpleexpr:
    | ATOM                                      {Atom $1}
    | FLOAT                               {Float $1}
    | simpleexpr Llist       {Node($1,$2)}
    | STRING                              {String(toStr $1)}
    | ANY                                 {Any}
    | ARRAY_END                           {Arr_End}
    | INT                                 { Int $1                    }
    | IDENT                               { Var $1                    }
    | TRUE                                {Bool true}
    | FALSE                               {Bool false}
    | LPAREN expr RPAREN                  {$2}
    | BEGIN_FUNCTION IDENT DEFINITION expr END_FUNCTION      { Fun ($2,$4)  }
    | BEGIN_FUNCTION IDENT separatelist DEFINITION expr END_FUNCTION      { Fun ($2, listToFun (List.rev $3) $5)  }
    | BEGIN_FUNCTION expr END_FUNCTION      { Exp($2)  }
    | UNIFY expr WITH matchlist {buildUnify ($2) (List.rev $4)}
  ;

  Llist:
    | Llist Llist            {Node($1,$2)}
    | ARRAY_LINK simpleexpr  {$2}

;


  tuplenode:
  | tuplenode tuplenode        {TupleNode($1,$2)}
  | SEPARATOR expr        {$2}
;
  matchcond :
    | PIPE expr ARROW expr {($2,$4,[])}
    | PIPE expr POSTCOND expr ARROW expr {($2,$4,[$6])}
  ;

  matchlist:
    | matchcond {[$1]}
    | matchlist matchcond {$2::$1}
  ;
  separatevar:
    | SEPARATOR IDENT  {$2}
  ;

  separatelist:
    | separatevar { [$1] }
    | separatelist separatevar {$2::$1}
  ;


  simpleexprlist:
  | simpleexpr { [$1] }
  | simpleexprlist simpleexpr  { $2 :: $1 }
    /* cette règle permet d'avoir une associativite a gauche */
;
