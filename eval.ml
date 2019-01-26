open Expr ;;

let rec is_prog e = match e with
    | Var _ -> true
    | Int _ -> true
    | Bool _ -> true
    | Arr_End   -> true
    | Float _   -> true
    | String s -> true
    | Atom   _  -> true
    | TupleEnd  -> true
    | Unify(a,b) -> is_prog a && is_prog b
    | UnifyExp(a,b,c)-> is_prog a && is_prog b && is_prog c
	| UnifyExpCond(a,b,c,d) -> is_prog a && is_prog b && is_prog c && is_prog d
    | Node(a,b) -> is_prog a && is_prog b
    | NamedTuple(n,e) -> is_prog e
    | Tuple(_,l)    -> is_prog l
    | TupleNode(a,b) -> (is_prog a) && (is_prog b)
    | Any -> true
    | Cond (cond,th,el) -> is_prog cond && is_prog th && is_prog el
    | Fun (_,e) -> is_prog e
    | Exp (e) -> is_prog e
    | RecFun (_,_,e,s) -> is_prog  e && is_prog s
    | NamedFun (_,_,e,s) -> is_prog  e && is_prog s
    | NamedExp (_,e,s) -> is_prog  e && is_prog s
    | App (e1,e2) -> is_prog  e1 && is_prog  e2
    | Mod(e1,e2) -> is_prog  e1 && is_prog  e2
    | Add (e1,e2) -> is_prog  e1 && is_prog  e2
    | Sub (e1,e2) -> is_prog  e1 && is_prog  e2
    | Mult (e1,e2) -> is_prog  e1 && is_prog  e2
    | Div (e1,e2) -> is_prog  e1 && is_prog  e2
    | Eq (e1,e2) -> is_prog  e1 && is_prog  e2
    | Inf (e1,e2) -> is_prog  e1 && is_prog  e2
    | Infeq (e1,e2) -> is_prog  e1 && is_prog  e2
    | Sup (e1,e2) -> is_prog  e1 && is_prog  e2
    | Supeq (e1,e2) -> is_prog  e1 && is_prog  e2
    | Diff (e1,e2) -> is_prog  e1 && is_prog  e2
    | And (e1,e2) -> is_prog  e1 && is_prog  e2
    | Or (e1,e2) -> is_prog  e1 && is_prog  e2
    | Not e1 -> is_prog  e1
    | UEnd   -> true
    | Print e -> is_prog e
    | Sequencer(e1,e2) -> is_prog e1 && is_prog e2
    | Neg e             -> is_prog e
	| ToInt		e -> is_prog e
	| ToFloat	e -> is_prog e
	| ToBool	e -> is_prog e
	| ToString	e ->is_prog e

let rec substVar  x v e =
    let part = substVar  x v in
    match e with
    | Var y -> if x = y then v else (Var y)
    | Int f -> Int f
    | Bool b -> Bool b
    | Float f -> Float f
    | String s -> String s
    | Atom s    -> Atom s
    | TupleEnd  -> TupleEnd
    | Cond (cond,th,el) -> Cond(part cond,part th,part el)
    | Arr_End   -> Arr_End
    | Node(a,b) -> Node(part a,part b)
    | App (e1, e2) -> App( part e1, part e2)
    | Fun (y,b) ->
        if x = y
        then Fun (y,b)
        else Fun(y, part b)
    | Exp b -> Exp(part b)
    | NamedExp (n,b,s) -> NamedExp(n,part b,part s)
    | NamedFun(n,ve,e,s) ->
        if x = n
        then begin Fun(ve,part e) end
        else NamedFun(n,ve,part e,part s)
    | RecFun(n,ve,e,s) ->
        if x = n
        then begin Fun(ve,part e) end
        else RecFun(n,ve,e,part s)
    | Add (e1, e2) -> Add(part e1, part e2)
    | Mod (e1, e2) -> Mod(part e1, part e2)
    | Mult (e1, e2) -> Mult(part e1, part e2)
    | Div (e1, e2) -> Div(part e1, part e2)
    | Sub (e1, e2) -> Sub(part e1, part e2)
    | Eq (e1, e2) -> Eq(part e1, part e2)
    | Diff(e1, e2) -> Diff(part e1, part e2)
    | Sup(e1, e2) -> Sup(part e1, part e2)
    | Supeq(e1, e2) -> Supeq(part e1, part e2)
    | Inf(e1, e2) -> Inf(part e1, part e2)
    | Infeq(e1, e2) -> Infeq(part e1, part e2)
    | And(e1, e2) -> And(part e1, part e2)
    | Or(e1, e2) -> Or(part e1, part e2)
    | Not e1 -> Not(part e1)
    | NamedTuple(n,e)   -> NamedTuple(n,part e)
    | Tuple (n,l)       -> Tuple(n,part l)
    | TupleNode(a,b) -> TupleNode(part a,part b)
    | Any       -> Any
	| Unify(a,b) -> Unify(part a,part b)
	| UnifyExp(a,b,c)->
        if ident_in a x
        then begin
            UnifyExp(a,b,part c)
        end
        else
            UnifyExp(a,part b,part c)
	| UnifyExpCond(a,b,c,d)->
        if ident_in a x
        then begin
            UnifyExpCond(a,b,c,part c)
        end
        else
            UnifyExpCond(a,part b,part c,part d)
    | UEnd             -> UEnd
    | Print e -> Print(part e)
    | Sequencer(e1,e2) -> Sequencer(part e1,part e2)
    | Neg e             -> Neg (part e)
	| ToInt		e -> ToInt (part e)
	| ToFloat	e ->  ToFloat (part e)
	| ToBool	e ->  ToBool (part e)
	| ToString	e -> ToString (part e)
;;

type unifier =
    | None
    | Empty
    | Assembly of (expr*expr)


let rec unify expr pattern =
    match (expr,pattern) with
        | (_,Var s) -> [Assembly(expr,Var s)]
        | (Int a,Int b) when a=b->[Empty]
        | (Float a,Float b) when a=b->[Empty]
        | (String a,String b) when a=b->[Empty]
        | (Bool a,Bool b) when a=b ->  [Empty]
        | (TupleEnd,TupleEnd)       -> [Empty]
        | (NamedTuple(n,e),NamedTuple(n2,e2)) when n=n2 -> unify e e2
        | (Tuple(na,a),Tuple(nb,b)) when na = nb -> unify a b
        | (Atom(a),Atom(b))   when a =b  -> [Empty]
        | (TupleNode(a1,b1),TupleNode(a2,b2)) -> (unify a1 a2) @ (unify b1 b2)
        | (Node(a1,b1),Node(a2,b2))           -> (unify a1 a2) @ (unify b1 b2)
        | (Arr_End,Arr_End)                   -> [Empty]
        | (_,Any)                              -> [Empty]
        | (Var _,_)                           -> failwith "Erreur variable non lié"
        | _                                   -> [None]
;;


let try_unify expr pattern =
    let unifications = unify expr pattern in
    let errorfind a = match a with | None -> true | _ -> false in
    if not (List.exists errorfind unifications)
    then begin
    (*Erase empty unification*)
        let cleaner a = match a with | Empty -> false | _ -> true in
        let filteredGroups = List.filter cleaner unifications in
        (*Verify the coherence of the unification*)
        let validate a b =
            match (a,b) with
            | (Assembly(e1,v1),Assembly(e2,v2)) when ((v1=v2) && (e1=e2)) -> false
            | (Assembly(e1,v1),Assembly(e2,v2)) when ((v1=v2) && (e1!=e2)) -> true
            | _ -> false
        in
        (*Erase already compared var*)
        let eraseOthers b a = match (b,a) with
            | (Assembly(_,name),Assembly(e,n)) when n=name-> false
            | _                     -> true
        in
        let validateAll var lst =
            List.exists (validate var) lst
        in
        let rec searchError lst =
            match lst with
            | [] -> []
            | a::b ->
                if not(validateAll a b)
                then begin
                    [a] @ (searchError (List.filter (eraseOthers a)b))
                end
                else begin
                    [None]
                end
        in
        let result = searchError filteredGroups in
        if( (result) = [None])
        then [None]
        else
            result
        end
    else begin
        [None]
    end


let rec concat a b =
    match a with
    | Arr_End -> b
    | Node(h,t) -> Node(h,(concat t b))
    | _ -> failwith "Concatenation impossible"



let rec eval ex =
    let subst a b c= substVar  a b c in
    let rec subAll lst exp =
        if List.length lst >0 then
        (
        match lst with
        | (Assembly(ex,Var a))::[] ->(substVar  a ex exp)
        | (Assembly(ex,Var a))::b -> (subAll b (substVar  a ex exp))
        | _ -> failwith "Error"
        )
        else
            exp
    in
    let rec unificationOp var pattern =
        match pattern with
        | UnifyExp(a,b,c) ->
            let result = try_unify var a in
            if (not(result = [None]))
            then begin
                let e = subAll result b in
                e
            end
            else
                unificationOp var c
        | UnifyExpCond(a,b,c,d) ->
            let result = try_unify var a in
            let cond = (eval b) in
            if (not(result = [None]) && cond=(Bool true))
            then
                subAll result c
            else
                unificationOp var d
        | UEnd -> failwith "Erreur, pattern non capturé"
        | _    -> failwith "?"
    in
    let boolCompare a b op =
            match (eval a,  eval b) with
                    (Bool i1, Bool i2) -> Bool( op i1 i2 )
            | (Any,_) -> Bool true
            | (_,Any) -> Bool true
            | _ -> failwith "erreur de type : booléens attendus" in
    match ex with
    | Int f -> Int f
    | Var v ->
        failwith ("Variable non liée : " ^ v)
    | Atom s -> Atom s
    | Bool b -> Bool b
    | Float f -> Float f
    | String s -> String s
    | Cond (cond,th,el) ->
            let value = eval cond in
        (
            match value with
            | Bool b -> if b =true
                        then eval th
                        else eval el
            |  Any -> Bool true
            | _     -> failwith "Erreur : booléen attendu"
        )
    | Add (e1,e2) ->
            (
            match (eval e1,  eval e2) with
            | (Int i1, Int i2) -> Int( i1 + i2 )
            | (Float i1,Float i2) ->Float(i1+.i2)
            | (Float i1,Int i2) ->Float(i1+.(float_of_int i2))
            | (Int i2,Float i1) ->Float(i1+.(float_of_int i2))
            | (String s1,String s2) -> String(s1 ^ s2)
            | (Node(a,b),Node(c,d)) -> concat e1 e2
            | _ -> failwith "erreur de type : nombres attendus"
            )
    | Mult (e1,e2) ->
                (
            match (eval e1,  eval e2) with
            | (Int i1, Int i2) -> Int( i1 * i2 )
            | (Float i1,Float i2) ->Float(i1*.i2)
            | (Float i1,Int i2) ->Float(i1*.(float_of_int i2))
            | (Int i2,Float i1) ->Float(i1*.(float_of_int i2))
            | _ -> failwith "erreur de type : nombres attendus"
            )
    | Div (e1,e2) ->
            (
            match (eval e1,  eval e2) with
            | (Int i1, Int i2) -> Int( i1 / i2 )
            | (Float i1,Float i2) ->Float(i1/.i2)
            | (Float i1,Int i2) ->Float(i1/.(float_of_int i2))
            | (Int i2,Float i1) ->Float(i1/.(float_of_int i2))
            | _ -> failwith "erreur de type : nombres attendus"
            )
    | Mod (e1,e2) ->
            (
            match eval e1,eval e2 with
            | (Int i1,Int 0) -> failwith "Erreur : modulo 0 impossible"
            | (Int i1, Int i2) -> Int( (i1 mod i2) )
            | _ -> failwith "erreur de type : nombres attendus"
            )
    | Sub (e1,e2) ->
            (
            match (eval e1,  eval e2) with
            | (Int i1, Int i2) -> Int( i1 - i2 )
            | (Float i1,Float i2) ->Float(i1-.i2)
            | (Float i1,Int i2) ->Float(i1-.(float_of_int i2))
            | (Int i2,Float i1) ->Float(i1-.(float_of_int i2))
            | _ -> failwith "erreur de type : nombres attendus"
            )
    | Eq (e1,e2) ->
        (
            match (e1,e2) with
            | (Bool a,Bool b) -> Bool( a= b )
            | (Int a,Int b) ->  Bool( a= b )
            | (Float a,Float b) ->  Bool( a=b)
            | (Float i1,Int i2) ->  Bool(i1=(float_of_int i2))
            | (Int i1,Float i2) ->  Bool((float_of_int i1)=i2)
            | (String s1,String s2) -> Bool(s1=s2)
            | (Node(a,b),Node(c,d)) ->
                    (
                        match (eval (Eq(a,c)),eval (Eq(b,d))) with
                        | (Bool true,Bool true) -> Bool true
                        | _     -> Bool false
                    )
            |  (Arr_End,Arr_End) -> Bool true
            |  (Node(a,b),Arr_End) -> Bool false
            | (Arr_End,Node(a,b)) -> Bool false
            | _ -> failwith "Erreur : types différents"
        )
    | Inf (e1,e2) ->
        (
            match (e1,e2) with
            | (Bool a,Bool b) -> Bool( a< b )
            | (Int a,Int b) ->  Bool( a< b )
            | (Float a,Float b) ->  Bool( a<b)
            | (Float i1,Int i2) ->  Bool(i1<(float_of_int i2))
            | (Int i1,Float i2) ->  Bool((float_of_int i1)<i2)
            | (Node(a,b),Node(c,d)) ->
                    (
                        match (eval (Eq(a,c)),eval (Eq(b,d))) with
                        | (Bool true,Bool true) -> Bool true
                        | _     -> Bool false
                    )
            |  (Arr_End,Arr_End) -> Bool true
            |  (Node(a,b),Arr_End) -> Bool false
            | (Arr_End,Node(a,b)) -> Bool false
            | _ -> failwith "Erreur : types différents"
        )
    | Infeq (e1,e2) ->
        (
            match (e1,e2) with
            | (Bool a,Bool b) -> Bool( a<= b )
            | (Int a,Int b) ->  Bool( a<= b )
            | (Float a,Float b) ->  Bool( a<=b)
            | (Float i1,Int i2) ->  Bool(i1<=(float_of_int i2))
            | (Int i1,Float i2) ->  Bool((float_of_int i1)<=i2)
            | (Node(a,b),Node(c,d)) ->
                    (
                        match (eval (Eq(a,c)),eval (Eq(b,d))) with
                        | (Bool true,Bool true) -> Bool true
                        | _     -> Bool false
                    )
            |  (Arr_End,Arr_End) -> Bool true
            |  (Node(a,b),Arr_End) -> Bool false
            | (Arr_End,Node(a,b)) -> Bool false
            | _ -> failwith "Erreur : types différents"
        )
    | Sup (e1,e2) ->
        (
            match (e1,e2) with
            | (Bool a,Bool b) -> Bool( a> b )
            | (Int a,Int b) ->  Bool( a> b )
            | (Float a,Float b) ->  Bool( a>b)
            | (Float i1,Int i2) ->  Bool(i1>(float_of_int i2))
            | (Int i1,Float i2) ->  Bool((float_of_int i1)>i2)
            | (Node(a,b),Node(c,d)) ->
                    (
                        match (eval (Eq(a,c)),eval (Eq(b,d))) with
                        | (Bool true,Bool true) -> Bool true
                        | _     -> Bool false
                    )
            |  (Arr_End,Arr_End) -> Bool true
            |  (Node(a,b),Arr_End) -> Bool false
            | (Arr_End,Node(a,b)) -> Bool false
            | _ -> failwith "Erreur : types différents"
        )
    | Supeq (e1,e2) ->
        (
            match (e1,e2) with
            | (Bool a,Bool b) -> Bool( a>= b )
            | (Int a,Int b) ->  Bool( a>= b )
            | (Float a,Float b) ->  Bool( a>=b)
            | (Float i1,Int i2) ->  Bool(i1>=(float_of_int i2))
            | (Int i1,Float i2) ->  Bool((float_of_int i1)>=i2)
            | (Node(a,b),Node(c,d)) ->
                    (
                        match (eval (Eq(a,c)),eval (Eq(b,d))) with
                        | (Bool true,Bool true) -> Bool true
                        | _     -> Bool false
                    )
            |  (Arr_End,Arr_End) -> Bool true
            |  (Node(a,b),Arr_End) -> Bool false
            | (Arr_End,Node(a,b)) -> Bool false
            | _ -> failwith "Erreur : types différents"
        )
    | Diff (e1,e2) ->
    (
            match (e1,e2) with
            | (Bool a,Bool b) -> Bool( a!= b )
            | (Int a,Int b) ->  Bool( a!= b )
            | (Float a,Float b) ->  Bool( a!=b)
            | (Float i1,Int i2) ->  Bool(i1!=(float_of_int i2))
            | (Int i1,Float i2) ->  Bool((float_of_int i1)!=i2)
            | (Node(a,b),Node(c,d)) ->
                    (
                        match (eval (Eq(a,c)),eval (Eq(b,d))) with
                        | (Bool true,Bool true) -> Bool true
                        | _     -> Bool false
                    )
            |  (Arr_End,Arr_End) -> Bool true
            |  (Node(a,b),Arr_End) -> Bool false
            | (Arr_End,Node(a,b)) -> Bool false
            | _ -> failwith "Erreur : types différents"
        )
    | And (e1,e2) -> boolCompare e1 e2 (&&)
    | Or (e1,e2) -> boolCompare e1 e2 (||)
    | Not e1 ->
        (
            match eval e1 with
            | Bool b -> Bool (not b)
            | _ -> failwith "erreur de type : booléen attArr_Endus"
        )
    | Fun (s,e) -> Fun (s,e)
    | Exp (e) ->    eval e
    | RecFun (n,v,s,se) ->
        let nRec = RecFun(n,v,s,s) in
        let nFun = Fun(v,nRec) in
        eval (subst n (eval nFun) se)
    | NamedFun (n,v,s,se) ->
        eval (subst n (Fun(v,s)) se)
    | NamedExp (n,s,se) ->
        eval (subst n s se)
    | App (e1,e2) ->
            let r2 = eval e2 in
            let r1 = eval e1 in
            (match r1 with
            | Fun(s,e) ->
                let b = subst s r2 e in
                eval b
            | _ -> failwith "erreur de type : fonction attendue")
    | Any -> Any
    | Arr_End -> Arr_End
    | Node(a,b) -> Node(eval a,eval b)
    | NamedTuple(a,b) -> NamedTuple(a,eval b)
    | Tuple(n,l)        ->Tuple(n,eval l)
    | TupleNode (a,b)   -> TupleNode(eval a,eval b)
    | Unify(a,b) ->
        (match a with
            | Var _ -> Unify(a,b)
            | _ -> eval (unificationOp a b)
        )
    | UnifyExp(a,b,c)-> failwith "?"
    | UnifyExpCond(a,b,c,d)-> failwith "?"
    | UEnd          -> failwith "Erreur"
    | Print e       -> print_exp (eval e); UEnd
    | Sequencer(e1,e2) -> (ignore (eval e1));eval e2
    | Neg e             ->
        (
            match eval e with
            | Int a -> Int(-a)
            | Float a -> Float(-.a)
            | _         -> failwith "Erreur : Nombre requis"
        )
    | TupleEnd -> TupleEnd
	| ToInt		e ->
        (
            match (eval e) with
            | Int e -> Int e
            | Float e -> Int(int_of_float e)
            | String   e -> Int(int_of_string e)
            | Bool true -> Int(0)
            | Bool false -> Int(1)
            | _ -> failwith "Erreur : entier,float,string ou booléen nécessaire"
        )
	| ToFloat	e ->
        (
            match (eval e) with
            | Int e -> Float (float_of_int e)
            | Float e -> Float e
            | String   e -> Float (float_of_string e)
            | Bool true -> Float (0.)
            | Bool false -> Float (1.)
            | _ -> failwith "Erreur : entier,float,string ou booléen nécessaire"
        )
	| ToBool	e ->
        (
            match (eval e) with
            | Int 0 -> Bool false
            | Int _ -> Bool true
            | Float 0.0 -> Bool false
            | Float _   -> Bool true
            | String  "true" -> Bool true
            | String "false" -> Bool false
            | Bool true -> Bool true
            | Bool false -> Bool true
            | _ -> failwith "Erreur : entier,float,string ou booléen nécessaire"
        )
	| ToString e ->
        (
            match eval e with
                | Int e -> String(string_of_int e)
                | Float e -> String(string_of_float e)
                | Bool false -> String("false")
                | Bool true -> String("true")
                | String e -> String(e)
            | _ -> failwith "Erreur : entier,float,string ou booléen nécessaire"
        )
;;

