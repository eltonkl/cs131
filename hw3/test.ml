
(* A simple test harness for the MOCaml interpreter. *)

(* put your tests here:
   each test is a pair of a MOCaml declaration and the expected
   result:
     - the MOCaml declaration is a string of exactly what you would type into the interpreter prompt,
       without the trailing ";;"
     - the expected result is a string of exactly what you expect the interpreter to print as a result
   use the string "dynamic type error" as the result if a DynamicTypeError is expected to be raised.
   use the string "match failure" as the result if a MatchFailure is expected to be raised.
   use the string "implement me" as the result if an ImplementMe exception is expected to be raised

   call the function runtests() to run these tests
*)
let tests =
    [
    (* YOU NEED TO ADD A LOT MORE TESTS! *)
        (* IntVal *)
        ("3", "3");
        (* BoolVal *)
        ("false", "false");
        (* let declaration *)
        ("let x = 34", "val x = 34");
        (* Var *)
        ("y", "dynamic type error");
        (* Var, IntVal, BinOp *)
        ("x + 4", "38");
        (* Negate, IntVal *)
        ("-5", "-5");
        ("-(-5)", "5");
        (* BinOp, IntVal, BoolVal *)
        ("5 - 3", "2");
        ("5 > 4", "true");
        (* If, IntVal, BinOp, BoolVal *)
        ("if 5 > 4 then 9 else 3", "9");
        (* Data, Tuple *)
        ("Node (Node 5, 6, Node 7)", "Node (Node 5, 6, Node 7)");
        (* Match, Tuple, Function, FunctionCall, BinOp, Var *)
        ("match ((function x -> x + 3), (function x -> x + 5)) with (x, y) -> (x 6) + (y 7) | x -> x 5", "21");
        (* let rec declaration, Match, Tuple, Data, Function, FunctionCall, Var *)
        ("let rec a (x, y) = match x with (c, d) -> c + d | Node -> Node y | z -> a y", "val a = <fun>");
        (* Var, IntVal, Tuple, Data, FunctionCall *)
        ("a (Node, 5)", "Node 5");
        (* "match failure" is expected because the recursive call to a will
         * consist of (a 6), and 6 does not match the parameter for the
         * function, as the function expects a tuple *)
        ("a (5, 6)", "match failure");
        ("a (5, (5, 6))", "match failure");
        ("a (5, (Node, 5))", "Node 5");
        ("a (5, (5, ((5, 6), 0)))", "11");
        (* Some Data tests *)
        ("Node Node 5", "dynamic type error");
        ("Node (Node 5)", "Node Node 5");
        ("Node (function x -> x + 5)", "Node <fun>");
    ]

(* The Test Harness
   You don't need to understand the code below.
*)
  
let testOne test env =
  let decl = main token (Lexing.from_string (test^";;")) in
  let res = evalDecl decl env in
  let str = print_result res in
  match res with
      (None,v) -> (str,env)
    | (Some x,v) -> (str, Env.add_binding x v env)
      
let test tests =
  let (results, finalEnv) =
    List.fold_left
      (fun (resultStrings, env) (test,expected) ->
	let (res,newenv) =
	  try testOne test env with
	      Parsing.Parse_error -> ("parse error",env)
	    | DynamicTypeError _ -> ("dynamic type error",env)
	    | MatchFailure -> ("match failure",env)
	    | ImplementMe s -> ("implement me",env) in
	(resultStrings@[res], newenv)
      )
      ([], Env.empty_env()) tests
  in
  List.iter2
    (fun (t,er) r ->
      let out = if er=r then "ok" else "expected " ^ er ^ " but got " ^ r in
      print_endline
	(t ^ "....................." ^ out))
      tests results

(* CALL THIS FUNCTION TO RUN THE TESTS *)
let runtests() = test tests
  
