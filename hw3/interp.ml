(* Name: Elton Leong

   UID:

   Others With Whom I Discussed Things: Neda Vesselinova
                                        Shalini Dangi
                                        James Wang

   Other Resources I Consulted:
    http://caml.inria.fr/pub/docs/oreilly-book/html/book-ora016.html
    http://caml.inria.fr/pub/docs/manual-ocaml/expr.html
    http://stackoverflow.com/questions/257605/ocaml-match-expression-inside-another-one
    http://ocaml-lib.sourceforge.net/doc/Option.html
    https://realworldocaml.org/v1/en/html/error-handling.html
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
    http://caml.inria.fr/resources/doc/guides/debug.en.html
*)

(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
   You should provide a useful error message.
*)
exception DynamicTypeError of string

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
    match (pat, value) with
    (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
    | (IntPat(i), IntVal(j)) when i = j -> Env.empty_env ()
    (* same thing but for boolean values *)
    | (BoolPat(i), BoolVal(j)) when i = j -> Env.empty_env ()
    (* match whatever, don't insert a binding as there is no given name *)
    | (WildcardPat, _) -> Env.empty_env ()
    (* create a name binding for s to the given value *)
    | (VarPat(s), _) -> Env.add_binding s value (Env.empty_env ())
    (* bind all patterns in the TuplePat to all values in the TupleVal, as long as
     * the TuplePat and TupleVal are of the same length (we want a surjective mapping) *)
    | (TuplePat(patl), TupleVal(vall)) when (List.length patl) = (List.length vall) ->
            List.fold_left2
            (fun menv mpat mval -> Env.combine_envs menv (patMatch mpat mval)) (Env.empty_env ()) patl vall
    (* Can only match a DataPat to a DataVal if their Constructors are the same,
     * and if their associated values can be matched together *)
    | (DataPat(i1, i2), DataVal(j1, j2)) when i1 = j1 ->
            begin
                match (i2, j2) with
                | (Some mpat, Some mval) -> patMatch mpat mval
                | (None, None) -> Env.empty_env ()
                | _ -> raise MatchFailure
            end
    | _ -> raise MatchFailure

(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
    match e with
    (* an integer constant evaluates to itself *)
    | IntConst(i) -> IntVal(i)
    (* a bool constant evaluates to itself *)
    | BoolConst(b) -> BoolVal(b)
    (* a var expression evaluates to the first thing found in the current
     * environment with the given var name *)
    | Var(s) ->
            begin
                try Env.lookup s env with
                | Env.NotBound -> raise (DynamicTypeError "Unbound value")
            end
    (* a BinOp is a binary operation on two integers
     * 5 operations are supported: +, -, *, =, > *)
    | BinOp(mex1, mop, mex2) ->
            let res1 = (evalExpr mex1 env) in
            let res2 = (evalExpr mex2 env) in
            begin
                match (res1, res2) with
                | (IntVal(i1), IntVal(i2)) ->
                        begin
                            match mop with
                            | Plus -> IntVal(i1 + i2)
                            | Minus -> IntVal(i1 - i2)
                            | Times -> IntVal(i1 * i2)
                            | Eq -> BoolVal(i1 = i2)
                            | Gt -> BoolVal(i1 > i2)
                        end
                | _ -> raise (DynamicTypeError "Cannot apply a binary operator to a non-integer")
            end
    (* Negation of expressions of the form mex: -(mex) *)
    | Negate(mex) ->
            begin
                match (evalExpr mex env) with
                | IntVal(i) -> IntVal(-i)
                | _ -> raise (DynamicTypeError "Cannot negate a non-integer value")
            end
    (* if (ifexp) then (thenexp) else (elseexp) *)
    | If(ifexp, thenexp, elseexp) ->
            begin
                match (evalExpr ifexp env) with
                | BoolVal(b) ->
                        if b then evalExpr thenexp env
                        else evalExpr elseexp env
                | _ -> raise (DynamicTypeError "If expression does not return a boolean value")
            end
    (* function mpat -> mex *)
    | Function(mpat, mex) ->
                (* A function receives a copy of the environment surrounding it when it was evaluated *)
                FunctionVal(None, mpat, mex, env)
    (* (mex1) (mex2)
     * mex1 must evaluate to a function (it can be a Var expression, or a Function expression),
     * then mex1 will then be called with (mex2) being bound to the pattern of the function, if
     * the pattern and mex2 can be successfully pattern matched *)
    | FunctionCall(mex1, mex2) ->
            let evalFunc fpat fexp fenv pval =
                evalExpr fexp (Env.combine_envs fenv (patMatch fpat pval))
            in
            let mval1 = evalExpr mex1 env in
            begin
                match mval1 with
                | FunctionVal(name, mpat, mexp, menv) ->
                        begin
                            let mval2 = evalExpr mex2 env in
                            match name with
                            (* If the function is recursive, bind the function name
                             * to its FunctionVal in the environment of the function *)
                            | Some s -> evalFunc mpat mexp (Env.add_binding s mval1 menv) mval2
                            | None -> evalFunc mpat mexp menv mval2
                        end
                | _ -> raise (DynamicTypeError "First expression is not a function")
            end
    (* match (mex) with (fst (List.head l)) -> (snd (List.head l)) | (fst (List.nth n l)) -> (snd (List.nth n l)) | ...
     * where l is a (mopat * moexpr) list:
     * if mex can be pattern matched to one of the patterns, then the
     * match expression evaluates to the expression corresponding to the pattern *)
    | Match(mex, l) ->
            let mval = (evalExpr mex env) in
            let rec tryMatch l =
                match l with
                | [] -> raise MatchFailure
                | h::t ->
                        try (snd h, patMatch (fst h) mval) with
                        | MatchFailure -> tryMatch t
            in
            let (matex, matenv) = tryMatch l in
            evalExpr matex (Env.combine_envs env matenv)
    (* (mex1, mex2, ..., mexn)
     * Evaluate all expressions into values for the tuple *)
    | Tuple(l) -> TupleVal(List.map (fun mex -> evalExpr mex env) l)
    (* (Constructor) <- No associated expression
     * (Constructor mex) <- (mex) associated with Constructor
     * Data types in mocaml can be associated with any movalue, or none at all.
     * This gives us the ability to do things like Node false "Node of BoolVal false",
     * Node ((function x -> x + 5) 5), which evaluates to Node 10 "Node of IntVal 10",
     * Node (Leaf 5) "Node of (Leaf of IntVal 5)", etc. *)
    | Data(s, o) ->
            match o with
            | Some mex -> DataVal(s, Some (evalExpr mex env))
            | None -> DataVal(s, None)


(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
    match d with
    (* a top-level expression has no name and is evaluated to a value *)
    | Expr(e) -> (None, evalExpr e env)
    (* bind the evaluation of mex to s *)
    | Let(s, mex) -> (Some s, evalExpr mex env)
    (* bind the evaluation of mex to s and register that it is a recursive
     * function *)
    | LetRec(s, mex) ->
                match (evalExpr mex env) with
                | FunctionVal(None, mpat, mex, menv) ->
                        (Some s, FunctionVal(Some s, mpat, mex, menv))
                (* This should never happen *)
                | _ -> raise (DynamicTypeError "Cannot match FunctionVal for LetRec")
