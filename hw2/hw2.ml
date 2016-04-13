(* Resources used: MSDN F# documentation
 *
 *)


exception ImplementMe

(* Problem 1: Vectors and Matrices *)

(* type aliases for vectors and matrices *)            
type vector = float list                                 
type matrix = vector list

let (vplus : vector -> vector -> vector) =
    fun v1 v2 ->
        List.map2 (fun f1 f2 -> f1 +. f2) v1 v2

let (mplus : matrix -> matrix -> matrix) =
    fun m1 m2 ->
        List.map2 (fun v1 v2 -> vplus v1 v2) m1 m2

let (dotprod : vector -> vector -> float) =
    fun v1 v2 ->
        List.fold_right2
        (fun f1 f2 a -> a +. (f1 *. f2)) v1 v2 0.0

let (transpose : matrix -> matrix) =
    fun m ->
        match m with
        | [] -> []
        | h::_ ->
                (* For the cth column in the matrix, *)
                List.mapi
                (
                    fun c _ ->
                    (* collect the cth element of every row into
                     * a row vector for the transpose. *)
                        List.map
                        (fun v -> List.nth v c) m
                ) h

let (mmult : matrix -> matrix -> matrix) =
    fun m1 m2 ->
        (* Get the transpose of m2 so we can use dotprod
         * to calculate individual entries in the result matrix. *)
        let t2 = transpose m2 in
        (* For every row in matrix 1, *)
        List.mapi
        (
            fun r v1 ->
                (* dot product the row with the cth column in matrix 2. *)
                List.map
                (fun c -> dotprod v1 c) t2
        ) m1

(* Problem 2: Calculators *)           

(* a type for arithmetic expressions *)
type op = Plus | Minus | Times | Divide
type exp = Num of float | BinOp of exp * op * exp

let (evalExp : exp -> float) =
  raise ImplementMe

(* a type for stack instructions *)	  
type instr = Push of float | Swap | Calculate of op

let (execute : instr list -> float) =
  raise ImplementMe

let (compile : exp -> instr list) =
  raise ImplementMe

let (decompile : instr list -> exp) =
  raise ImplementMe

(* EXTRA CREDIT *)        
let (compileOpt : exp -> (instr list * int)) =
  raise ImplementMe

