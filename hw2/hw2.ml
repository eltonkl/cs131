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
        List.mapi
        (
            fun r v1 ->
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

let swapFirstTwo lst =
    let tail = List.tl lst in
    [List.hd tail; List.hd lst] @ List.tl tail

let (execute : instr list -> float) =
    fun il ->
        let rec helper =
            fun s ilst ->
                match ilst with
                | [] -> s
                | h::t ->
                        match h with
                        | Push f -> helper (f::s) t
                        | Swap -> helper (swapFirstTwo s) t
                        | Calculate o ->
                                let tail = List.tl s in
                                let r = evalExp (BinOp (Num (List.hd tail), o, Num (List.hd s))) in
                                helper (r::(List.tl tail)) t
        in
        List.hd (helper [] il)

let rec (compile : exp -> instr list) =
    fun e ->
        match e with
        | Num f -> [Push f]
        | BinOp (e1, o, e2) -> (compile e1) @ (compile e2) @ [Calculate o]

let (decompile : instr list -> exp) =
    fun il ->
        let rec helper =
            fun elst ilst ->
                match ilst with
                | [] -> elst
                | h::t ->
                        match h with
                        | Push f -> helper ([Num f] @ elst) t
                        | Swap -> helper (swapFirstTwo elst) t
                        | Calculate o ->
                                let tail = List.tl elst in
                                let exp = (BinOp (List.hd tail, o, List.hd elst)) in
                                helper (exp::(List.tl tail)) t
        in
        List.hd (helper [] il)

(* EXTRA CREDIT *)        
let (compileOpt : exp -> (instr list * int)) =
  raise ImplementMe

