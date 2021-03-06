(* Resources used: MSDN F# documentation
 * and http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html
 *
 * People I talked to:
     * Shalini Dangi
     * Neda Vesselinova
     * James Wang
 *)


exception ImplementMe

(* Problem 1: Vectors and Matrices *)

(* type aliases for vectors and matrices *)            
type vector = float list                                 
type matrix = vector list

let (vplus : vector -> vector -> vector) =
    fun v1 v2 ->
        List.map2 (+.) v1 v2

let (mplus : matrix -> matrix -> matrix) =
    fun m1 m2 ->
        List.map2 vplus m1 m2

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
        List.map
        (
            fun v1 ->
                (* collect the dot products of the row with 
                 * every column in matrix 2 into a vector. *)
                List.map
                (fun c -> dotprod v1 c) t2
        ) m1

(* Problem 2: Calculators *)           

(* a type for arithmetic expressions *)
type op = Plus | Minus | Times | Divide
type exp = Num of float | BinOp of exp * op * exp

let rec (evalExp : exp -> float) =
    fun e ->
        match e with
        | Num f -> f
        | BinOp (e1, o, e2) ->
                let r1 = evalExp e1 in
                let r2 = evalExp e2 in
                match o with
                | Plus -> r1 +. r2
                | Minus -> r1 -. r2
                | Times -> r1 *. r2
                | Divide -> r1 /. r2

(* a type for stack instructions *)	  
type instr = Push of float | Swap | Calculate of op

(* Helper function for swapping first two elements of a list *)
let swapFirstTwo lst =
    match lst with
    | h1::h2::t -> h2::h1::t
    | _ -> failwith "List does not have at least two elements"

let (execute : instr list -> float) =
    fun il ->
        let rec helper =
            (* where s is the "stack", and ilst is the instruction list *)
            (* the first element of s is the top of the stack *)
            fun s i ->
                match i with
                | Push f -> f::s
                | Swap -> swapFirstTwo s
                | Calculate o ->
                        let tail = List.tl s in
                        (* evaluation order is exp2 op exp1 *)
                        let r = evalExp (BinOp (Num (List.hd tail), o, Num (List.hd s))) in
                        (r::(List.tl tail))
        in
        List.hd (List.fold_left helper [] il)

let rec (compile : exp -> instr list) =
    fun e ->
        match e with
        | Num f -> [Push f]
        | BinOp (e1, o, e2) -> (compile e1) @ (compile e2) @ [Calculate o]

let (decompile : instr list -> exp) =
    fun il ->
        let rec helper =
            fun elst i ->
                match i with
                | Push f -> (Num f)::elst
                | Swap -> swapFirstTwo elst
                | Calculate o ->
                        let tail = List.tl elst in
                        let exp = (BinOp (List.hd tail, o, List.hd elst)) in
                        exp::(List.tl tail)
        in
        List.hd (List.fold_left helper [] il)

(* EXTRA CREDIT *)        
let rec (compileOpt : exp -> (instr list * int)) =
    fun e ->
        match e with
        | Num f -> ([Push f], 1)
        | BinOp (e1, o, e2) ->
                let (il1, sz1) = compileOpt e1 in
                let (il2, sz2) = compileOpt e2 in
                (* We only compile the right operand first if it requires
                 * more stack space than s1 *)
                if sz2 > sz1 then
                    match o with
                    (* Only swap for non-commutative operations *)
                    | Minus | Divide ->
                            (il2 @ il1 @ [Swap; Calculate o], 1 + sz1)
                    | _ ->
                            (il2 @ il1 @ [Calculate o], 1 + sz1)
                else
                    (il1 @ il2 @ [Calculate o], 1 + sz2)
