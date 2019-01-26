 {
        open Parser        (* The type token is defined in parser.mli *)
	exception Eof

 }
  rule token = parse
    | "\""['a'-'z''A'-'Z' ' ' '\t']+"\""     as lxm     {STRING(lxm)}
    |  [' ' '\t']     { token lexbuf }     (* skip blanks *)
    | ['\n' ]        { token lexbuf }
    | ['0'-'9']'.'['0'-'9']*     as lxm {FLOAT(float_of_string lxm)}
    | ['1'-'9']+['0'-'9']'.'['0'-'9']*     as lxm {FLOAT(float_of_string lxm)}
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | "END"           {EOF}
    | ['A'-'Z']['a'-'z''A'-'Z']*'\\' as lxm { NAMEDFUN(lxm) }
    | ['A'-'Z']['a'-'z''A'-'Z']*'@''\\' as lxm { RECNAME(lxm) }
    | ['a'-'z']['a'-'z''A'-'Z']* as lxm {
        match lxm with
        | "true" -> TRUE
        | "false" -> FALSE
        | "if"  -> IF
        | "then" -> THEN
        | "else" -> ELSE
        | "end" -> END
        | "unify" -> UNIFY
        | "match" -> MATCHEX
        | "with"  -> WITH
        | "print" -> PRINT
        | "int"   -> TOINT
        | "bool"   -> TOBOOL
        | "float"   -> TOFLOAT
        | "string"   -> TOSTRING
        | _ -> ATOM(lxm) }
    | ['A'-'Z']['a'-'z''A'-'Z']* as lxm {IDENT(lxm) }
    | ':'           {SEQUENCER}
    | '='           {EQ}
    | "!="          {DIFF}
    | ":-"          {POSTCOND}
    | "&&"          {AND}
    | "||"          {OR}
    | '!'           {NOT}
    | '>'           {SUP}
    | '<'           {INF}
    | ">="          {SUPEQ}
    | "<="          {INFEQ}
	| '\\'			{BEGIN_FUNCTION}
	| '.'			{DEFINITION}
	| ';'			{END_FUNCTION}
    | '('           {LPAREN}
    | ')'           {RPAREN}
    | '+'           {PLUS}
    | '-'           {SUB}
    | '/'           {DIV}
    | '%'           {MODULO}
    | '*'           {MULT}
    | '_'           {ANY}
    | eof            { EOF }
    | '@'           {REC}
    | ','           {SEPARATOR}
    | "->"          {ARROW}
    | "[]"          {ARRAY_END}
    | "::"          {ARRAY_LINK}
    | '|'           {PIPE}
    | _             {token lexbuf}


