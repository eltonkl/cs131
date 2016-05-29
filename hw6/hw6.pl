/* Duplicate of an empty list is itself */
duplist([], []).
/* The head must be duplicated and the tail must be a duplist */
/* duplist(.(H, T), .(H, .(H, T2))) :- duplist(T, T2). */
duplist([H|T], [H|[H|T2]]) :- duplist(T, T2).

/* Empty list is a subsequence of any list */
subseq([], A) :- list(A).
/* Heads of both lists match, so their tails must be subsequences */
subseq([H|T], [H|T2]) :- subseq(T, T2).
/* Heads do not match, so the given list must be a subsequence of the tail of the second list */
subseq([H|T], [_|T2]) :- subseq([H|T], T2).

/* Modulo 10 adder */
carry_add(Num1, Num2, CIn, Result, COut) :-
    CIn #=< 1, COut #=< 1, Result #< 10,
    Result #= (Num1 + Num2 + CIn) rem 10,
    COut #= (Num1 + Num2 + CIn) // 10.

/* Remove trailing zero from a two element list */
remove_trailing_zero([X, 0], [X]).
remove_trailing_zero([X, Y], [X, Y]) :- fd_domain(Y, 1, 9).

/* Sum numbers, with digits of numbers being ordered backwards ([2, 4] + [3, 3] = [5, 7]) */
sum([], [], [], 0, 0).
sum([], [], [1], 1, 0).
sum([], [X], Result, CIn, 0) :-
    carry_add(0, X, CIn, Res, COut),
    remove_trailing_zero([Res, COut], Result).
sum([X], [], Result, CIn, 0) :- sum([], [X], Result, CIn, 0).
sum([H1|T1], [H2|T2], [Res|ResT], CIn, COut) :-
    carry_add(H1, H2, CIn, Res, CMid),
    sum(T1, T2, ResT, CMid, COut).

verbalarithmetic([All1|AllT], [Term1|Term1T], [Term2|Term2T], [Sum|SumT]) :-
    /* Given constraints */
    Term1 #\= 0, Term2 #\= 0, Sum #\= 0,
    fd_all_different([All1|AllT]),
    fd_domain([All1|AllT], 0, 9),
    /* As sum/5 expects numbers in the opposite order, reverse the lists */
    reverse([Term1|Term1T], T1), reverse([Term2|Term2T], T2), reverse([Sum|SumT], S),
    /* word1 + word2 = word3 */
    sum(T1, T2, S, 0, 0),
    /* Need a value to actually be assigned to all of the variables  */
    fd_labeling([All1|AllT]).

/* Limiting A, B, and C from being none significantly speeds up the evaluation */
move(world([A|AT], B, C, none), pickup(A, stack1), world(AT, B, C, A)) :- A \== none.
move(world(A, [B|BT], C, none), pickup(B, stack2), world(A, BT, C, B)) :- B \== none.
move(world(A, B, [C|CT], none), pickup(C, stack3), world(A, B, CT, C)) :- C \== none.

move(world(AT, B, C, A), putdown(A, stack1), world([A|AT], B, C, none)) :- A \== none.
move(world(A, BT, C, B), putdown(B, stack2), world(A, [B|BT], C, none)) :- B \== none.
move(world(A, B, CT, C), putdown(C, stack3), world(A, B, [C|CT], none)) :- C \== none.

blocksworld(world(A, B, C, D), [], world(A, B, C, D)).
blocksworld(world(A, B, C, D), [Action|T], world(A2, B2, C2, D2)) :-
    move(world(A, B, C, D), Action, world(AB, BB, CB, DB)),
    blocksworld(world(AB, BB, CB, DB), T, world(A2, B2, C2, D2)).
