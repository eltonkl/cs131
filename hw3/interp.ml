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
    | (BoolPat(i), BoolVal(j)) when i = j -> Env.empty_env ()
    | (WildcardPat, _) -> Env.empty_env ()
    | (VarPat(s), _) -> Env.add_binding s value (Env.empty_env ())
    | (TuplePat(patl), TupleVal(vall)) when (List.length patl) = (List.length vall) ->
            List.fold_left2
            (fun menv mpat mval -> Env.combine_envs menv (patMatch mpat mval)) (Env.empty_env ()) patl vall
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
    | BoolConst(b) -> BoolVal(b)
    | Var(s) ->
            begin
                try Env.lookup s env with
                | Env.NotBound -> raise (DynamicTypeError "Unbound value")
            end
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
    | Negate(mex) ->
            begin
                match (evalExpr mex env) with
                | IntVal(i) -> IntVal(-1 * i)
                | _ -> raise (DynamicTypeError "Cannot negate a non-integer value")
            end
    | If(ifexp, thenexp, elseexp) ->
            begin
                match (evalExpr ifexp env) with
                | BoolVal(b) ->
                        if b then evalExpr thenexp env
                        else evalExpr elseexp env
                | _ -> raise (DynamicTypeError "If expression does not return a boolean value")
            end
    | Function(mpat, mex) ->
                (* A function receives a copy of the environment surrounding it when it was evaluated *)
                FunctionVal(None, mpat, mex, env)
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
                            | Some s -> evalFunc mpat mexp (Env.add_binding s mval1 menv) mval2
                            | None -> evalFunc mpat mexp menv mval2
                        end
                | _ -> raise (DynamicTypeError "First expression is not a function")
            end
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
    (* Use map to evaluate all mocaml expressions into values for the tuple *)
    | Tuple(l) -> TupleVal(List.map (fun mex -> evalExpr mex env) l)
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
    | Let(s, mex) -> (Some s, evalExpr mex env)
    | LetRec(s, mex) ->
                match (evalExpr mex env) with
                | FunctionVal(None, mpat, mex, menv) ->
                        (Some s, FunctionVal(Some s, mpat, mex, menv))
                | _ -> raise MatchFailure
