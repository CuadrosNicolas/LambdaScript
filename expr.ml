

type expr =
	(*Types *)
	| Var of string
	| Int of int
	| Bool of bool
	| Float of float
	| String of string
	| Neg	of expr
	(*List composants*)
	| Arr_End
	| Node 		of expr*expr
	(*Tuple *)
	| Atom		of string
	| NamedTuple of string*expr
	| Tuple 	of int*expr
	| TupleNode		of expr*expr
	| TupleEnd
	(*Functions *)
	| NamedFun 	of string * string * expr * expr
	| RecFun 	of string * string * expr * expr
	| Exp 		of expr
	| NamedExp 	of string * expr * expr
	| Fun		of string * expr
	| App 		of expr * expr
	(*Int operations*)
	| Add 		of expr * expr
	| Sub 		of expr * expr
	| Div 		of expr * expr
	| Mult 		of expr * expr
	| Inf 		of expr * expr
	| Mod		of expr * expr
	(*Comparison operators *)
	| Infeq 	of expr * expr
	| Sup 		of expr * expr
	| Eq 		of expr * expr
	| Supeq 	of expr * expr
	| Diff 		of expr * expr
	(*Bool operations*)
	| Or 		of expr * expr
	| And 		of expr * expr
	| Not 		of expr
	(**Conditions *)
	| Cond 		of expr * expr * expr
	| Any
	(*Unification*)
					(*Varible / Unifications exp *)
	| Unify 	of expr * expr
	| UnifyExp	of expr*expr*expr
	| UnifyExpCond of expr*expr*expr*expr
	| UEnd
	(**Inbuild function*)
	| Print    of expr
	| Sequencer of expr * expr
	(*Conversions*)
	| ToInt		of expr
	| ToFloat		of expr
	| ToBool		of expr
	| ToString		of expr
;;

let rec ident_in exp name =
	match exp with
    | Var a -> if a = name then true else false
    | Int _ -> false
    | Bool _ -> false
    | Arr_End   -> false
    | UEnd   -> false
    | Any -> false
	| Float  _ -> false
	| String _ -> false
	| Atom _-> false
	| TupleEnd -> false
    | Unify(a,b) -> ident_in a name || ident_in b name
    | UnifyExp(a,b,c)-> ident_in a name || ident_in b name|| ident_in c name
    | UnifyExpCond(a,b,c,d)-> ident_in a name || ident_in b name|| ident_in c name || ident_in d name
    | Node(a,b) -> ident_in a name|| ident_in b name
	| NamedTuple (n,s) -> ident_in s name
    | Tuple(_,l)    -> ident_in l name
    | TupleNode(a,b) -> (ident_in a name) || (ident_in b name)
    | Cond (cond,th,el) -> ident_in cond name || ident_in th name || ident_in el name
    | Fun (a,e) -> if a = name then true else ident_in e name
    | Exp (e) -> ident_in e name
    | RecFun (_,n,e,s) -> if n = name then true else ident_in  e name || ident_in s name
    | NamedFun (_,n,e,s) -> if n = name then true else ident_in  e name|| ident_in s name
    | NamedExp (_,e,s) -> ident_in  e name || ident_in s name
    | App (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | Add (e1,e2) -> ident_in  e1 name || ident_in  e2 name
	| Mod (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | Sub (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | Mult (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | Div (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | Eq (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | Inf (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | Infeq (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | Sup (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | Supeq (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | Diff (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | And (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | Or (e1,e2) -> ident_in  e1 name || ident_in  e2 name
    | Not e1 -> ident_in  e1 name
	| Print e -> ident_in e name
	| Sequencer(e1,e2) -> ident_in e1 name || ident_in e2 name
	| Neg e				-> ident_in e name
	| ToInt		e -> ident_in e name
	| ToFloat	e -> ident_in e name
	| ToBool	e -> ident_in e name
	| ToString	e -> ident_in e name



let rec buildUnifyStep l =
		match l with
		| (a,b,c::[])::tl -> UnifyExpCond(a,b,c,buildUnifyStep tl)
		| (a,b,[])::tl ->  UnifyExp(a,b,buildUnifyStep tl)
		| []		-> UEnd
		| _			-> failwith "Parsing error"

let buildUnify var l =
	Unify (var,(buildUnifyStep l))



let rec indenter count =
 	 match count with
		| 0 -> ""
		| _ -> "\t"^(indenter (count-1))
let rec toString e count =
	let rec tupleToString e =
		match e with
		| TupleNode(a,b)	-> (toString a 0) ^ "," ^
			(
				match b with
					| TupleEnd -> ")"
					| TupleNode(a,c)	-> tupleToString b
					| _			->	toString b 0
			)
		| _	-> ""
	in
  	let opString e1 e2 str =toString e1 0 ^ " "^str^" " ^ toString e2 0 in
  	let desc =
	match e with
		| Atom s -> s
		| Var s -> s
		| String s -> "\""^s^"\""
		| Float f -> string_of_float f
		| Cond (cond,th,el) 	-> "if "^toString cond 0
										^"\n"^(indenter (count+1))^"then \n"^toString th (count+2)
										^"\n"^(indenter (count+1))^"else \n"^toString el (count+2)
		| Bool b 				->
									(
									match b with
									| true -> "true"
									| false -> "false"
									)
		| Int i 				-> string_of_int i
		| Arr_End					-> "[]"
		| Node(a,b)				-> (toString a 0) ^ "::" ^ (toString b 0)
		| NamedTuple(n,e)			-> n ^ "(" ^ (toString e 0)
		| Tuple(n,l)			-> (string_of_int n) ^ ":("^ (tupleToString l)
		| TupleNode(a,b)				-> "(" ^ (tupleToString e)
		| TupleEnd				-> ")"
		| App (e1, e2) 			-> "(" ^ toString e1 0 ^ " : " ^ toString e2 0^ ")"
		| Fun (x, b) 			-> "fun " ^ x ^ " -> \n" ^ toString b (count+1)
		| Exp (b) 				-> "exp -> \n" ^ toString b (count+1)
		| NamedFun (n,x, b,a) 	-> "fun@"^n^" : " ^ x ^ " -> " ^"\n"^toString b (count+1)^"\n"^ toString a count
		| RecFun (n,x, b,a) 	-> "rec@"^n^" : " ^ x ^ " -> " ^"\n"^toString b (count+1)^"\n"^ toString a count
		| NamedExp (n,b,a) 		-> "Exp@"^n^" : " ^ " -> " ^"\n"^toString b (count+1)^"\n"^ toString a count
		| Add (e1,e2) 			-> opString e1 e2 "+"
		| Mod (e1,e2) 			-> opString e1 e2 "%"
		| Mult (e1,e2) 			-> opString e1 e2 "*"
		| Div (e1,e2) 			-> opString e1 e2 "/"
		| Eq (e1,e2)  			-> opString e1 e2 "="
		| Inf (e1,e2)  			-> opString e1 e2 "<"
		| Infeq (e1,e2)  		-> opString e1 e2 "<="
		| Sup (e1,e2)  			-> opString e1 e2 ">"
		| Supeq (e1,e2) 		-> opString e1 e2 ">="
		| Diff (e1,e2)  		-> opString e1 e2 "!="
		| Sub (e1,e2) 			-> opString e1 e2 "-"
		| And (e1,e2) 			-> opString e1 e2 "and"
		| Or (e1,e2) 			-> opString e1 e2 "or"
		| Neg e					-> "-" ^ (toString e 0)
		| Not e1      			-> "not("^toString e1 0^")"
		| Any         			-> "_"
		| Unify(a,b) -> "Unify " ^ (toString a 0) ^ " with\n" ^ (toString b (count+1))
		| UnifyExp(a,b,c)-> "| " ^ (toString a 0) ^ " -> " ^ (toString b 0) ^ "\n" ^ (toString c count)
		| UnifyExpCond(a,b,c,d)-> "| " ^ (toString a 0) ^" :- "^(toString b 0)^ " -> " ^ (toString c 0) ^ "\n" ^ (toString d count)
		| UEnd			-> ""
		| Print e -> "Print("^(toString e 0)^")"
		| Sequencer(e1,e2) -> (toString e1 0) ^ "\n: " ^ (toString e2 0)
	| ToInt		e -> "Int("^(toString e 0)^")"
	| ToFloat	e -> "Float("^(toString e 0)^")"
	| ToBool	e -> "Bool("^(toString e 0)^")"
	| ToString e -> "String("^(toString e 0)^")"
  in
  (indenter count) ^ desc

let rec tostring e = toString e 0

let print_exp exp = print_endline (tostring exp)