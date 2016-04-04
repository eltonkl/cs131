#use "hw1.ml";;

assert ((member 1 [1;3;5]) = true);;
assert ((member 1 [3;5;7]) = false);;

let test =
    let temp = add 5 [1;3;4] in
    if not (List.mem 5 temp) then failwith "add is broken"
    else

    let temp = add 5 [1;3;5] in
    if not (temp = [1;3;5]) then failwith "add allows duplicates"
    else

    let temp = union [1;3;5] [1;3;5] in
    if not (temp = [1;3;5]) then failwith "union is broken"
    else

    let temp = union [1;3;5] [2;4;6] in
    if not (List.mem 1 temp && List.mem 2 temp && List.mem 3 temp
            && List.mem 4 temp && List.mem 5 temp && List.mem 6 temp)
    then failwith "union is broken 2"
    else

    let temp = fastUnion [1;3;5] [2;4;6] in
    if not (temp = [1;2;3;4;5;6]) then failwith "fastUnion is broken"
    else

    let temp = intersection [1;3;5] [3] in
    if not (temp = [3]) then failwith "intersection is broken"
    else

    let temp = intersection [] [] in
    if not (temp = []) then failwith "intersection is broken 2"
    else

    let temp = setify [1;1;5;5;5;5;5;5;5;6;6;6;6;6;6] in
    if not (temp = [1;5;6]) then failwith "setify is broken"
    else

    let temp = powerset [1;2;3] in
    if false then failwith "Test not implemented"
