transpose(Ms, Ts) :-
        (   Ms = [] -> Ts = []
        ;   Ms = [F|_],
            transpose(F, Ms, Ts)
        ).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

/* The above is not written by me, I take no credit for it, 
it is from the SWI-Prolog implementation:
https://github.com/lamby/pkg-swi-prolog/blob/master/library/clp/clpfd.pl */

counts(_,_,_,_).

plain_base_row(0,[]).
plain_base_row(X,[H|T]):-
    H is X,
    X1 is X-1,
    plain_base_row(X1, T),!.


plain_row([], _).
plain_row([H|Tail],Master):-
    select(H,Master,New),
    plain_row(Tail, New).


plain_col_valid([], _).
plain_col_valid([H|TranSol], Master):-
    plain_row(H,Master),
    plain_col_valid(TranSol,Master).


plain_by_col(_, _, [],_).
plain_by_col(N, Beg, [H|Sol], Master):-
    length(H,N),
    plain_row(H,Master),
    append(Beg,[H],Curr),
    transpose(Curr,CurrCol),
    plain_col_valid(CurrCol,Master),
    plain_by_col(N, Curr,Sol,Master).

plain_by_row(N, Sol,Master):-
    length(Sol,N),
    !,
    plain_by_col(N, 0, Sol,Master).

plain_tower(N,T,C):-
    C = counts(Top,Bottom,Left,Right),
    %length(Master,N),
    plain_base_row(N,Master),!,
    length(T,N),
    !,
    plain_by_col(N, [], T,Master),
    plain_view_row(T,Left,Right),
    transpose(T,TranSol),
    plain_view_row(TranSol,Top,Bottom).



plain_view_row([],[],[]).
plain_view_row([H|Rest],[Left|LeftTail],[Right|RightTail]):-
    plain_count(0, True1, H),
    sum_list(True1,Left),
    reverse(H,RevH),
    plain_count(0,True2,RevH),
    sum_list(True2,Right),
    plain_view_row(Rest,LeftTail,RightTail).

plain_count(_,[],[]).
plain_count(CurrMax, [True|OtherTrue], [H|Rest]):-
    CurrMax < H,
    True is 1,
    plain_count(H, OtherTrue, Rest).
plain_count(CurrMax, [True|OtherTrue], [H|Rest]):-
    CurrMax >= H,
    True is 0,
    plain_count(CurrMax, OtherTrue,Rest).

