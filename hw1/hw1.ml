(* Resources used: material from CS 131 discussion 1B
 * and from lecture *)

exception ImplementMe

(* Problem 1 *)
            
let rec (member : 'a -> 'a list -> bool) =
    fun x s ->
        match s with
        | [] -> false
        | h::t ->
                if x = h then true
                else member x t

let (add : 'a -> 'a list -> 'a list) =
    fun x s ->
        if member x s then s
        else s @ [x]

let rec (union : 'a list -> 'a list -> 'a list) =
    fun s1 s2 ->
        match s2 with
        | [] -> s1
        | h::t -> union (add h s1) t

let rec (fastUnion : 'a list -> 'a list -> 'a list) =
    fun s1 s2 ->
        match s1 with
        | [] -> s2
        | h1::t1 ->
                match s2 with
                | [] -> s1
                | h2::t2 ->
                        if h1 < h2 then h1::(fastUnion t1 s2)
                        else if h1 > h2 then h2::(fastUnion t2 s1)
                        else h1::(fastUnion t1 t2)
 
let (intersection : 'a list -> 'a list -> 'a list) =
    fun s1 s2 ->
        List.filter (function x -> member x s2) s1
                
let rec (setify : 'a list -> 'a list) =
    fun l ->
        match l with
        | [] -> l
        | h::t ->
                if member h t then setify t
                else h::(setify t)

let rec (powerset : 'a list -> 'a list list) =
    fun s ->
        let rec merge l ll =
            match ll with
            | [] -> l
            | h::t -> merge (l @ h) t
        in
        let rec remove x s =
            match s with
            | [] -> []
            | h::t ->
                    if h = x then t
                    else h::(remove x t)
        in
        let n_minus_one_subsets s =
            List.map (function x -> remove x s) s
        in
        let rec helper l =
            match l with
            | [] -> []
            | h::t ->
                    let subsets = n_minus_one_subsets l in
                    (merge [l] (List.map helper subsets))
        in
        setify ([[]] @ helper s)

(* Problem 2 *)
        
let (partition : ('a -> bool) -> 'a list -> 'a list * 'a list) =
    fun pred lst ->
        match lst with
        | [] -> ([], [])
        | h::t ->
                let rec helper orig l1 l2 =
                    match orig with
                    | [] -> (l1, l2)
                    | h::t ->
                            if pred h then helper t (l1 @ [h]) l2
                            else helper t l1 (l2 @ [h])
                in
                helper lst [] []

let rec (whle : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a) =
    fun p f x ->
        if p x then whle p f (f x)
        else x

let rec (pow : int -> ('a -> 'a) -> ('a -> 'a)) =
    fun n f ->
        match n with
        | 0 -> (function x -> x)
        | i -> (function x -> f ((pow (i - 1) f) x))
