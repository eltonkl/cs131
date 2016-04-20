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
      (IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
    | _ -> raise (ImplementMe "pattern matching not implemented")

    
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
    | BinOp(moex1, mop, moex2) ->
            let res1 = (evalExpr moex1 env) in
            let res2 = (evalExpr moex2 env) in
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
    | Negate(moex) ->
            begin
                match (evalExpr moex env) with
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
    | Function(mpat, moex) ->
            begin
                match mpat with
                (* A function receives a copy of the environment surrounding it when it was evaluated *)
                | VarPat(_) | TuplePat(_) -> FunctionVal(None, mpat, moex, env)
                | _ -> raise (DynamicTypeError "Function parameter does not match a VarPat or TuplePat")
            end
    | FunctionCall(moex1, moex2) ->
            begin
                (* Bind, in menv, the given parameters as defined in mval to their names
                 * as defined by the parameter pattern mpat *)
                let rec bindInEnv menv mpat mval =
                    match mpat with
                    | VarPat(s) -> Env.add_binding s mval menv
                    | TuplePat(patl) -> 
                            begin
                                match mval with
                                | TupleVal(vall) ->
                                        if (List.length patl) != (List.length vall) then
                                            raise (DynamicTypeError "Tuple is not of the correct length")
                                        else
                                            List.fold_left2 bindInEnv menv patl vall
                                | _ -> raise (DynamicTypeError "Tuple expected")
                            end
                    | _ -> raise (DynamicTypeError "This should never happen")
                in
                let evalFunc fpat fexp fenv pval =
                    evalExpr fexp (bindInEnv fenv fpat pval)
                in
                let mval1 = evalExpr moex1 env in
                match mval1 with
                | FunctionVal(name, mpat, mexp, menv) ->
                        begin
                            let pval = (evalExpr moex2 env) in
                            match name with
                            | Some s -> evalFunc mpat mexp (Env.add_binding s mval1 menv) pval
                            | None -> evalFunc mpat mexp menv pval
                        end
                | _ -> raise (DynamicTypeError "First expression is not a function")
            end
    (*| Match(mexp, l) -> *)
    (* Use map to evaluate all mocaml expressions into values for the tuple *)
    | Tuple(l) -> TupleVal(List.map (fun mexp -> evalExpr mexp env) l)
    | _ -> raise (ImplementMe "expression evaluation not implemented")


(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
      (* a top-level expression has no name and is evaluated to a value *)
      Expr(e) -> (None, evalExpr e env)
    | _ -> raise (ImplementMe "let and let rec not implemented")

