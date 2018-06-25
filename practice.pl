shift_left([_|R], R).

shift_right([_], []).
shift_right([H|T], [H|RT]):-
    shift_right(T, RT).

shift_left_circular([L|R], RL):-
    append(R,[L],RL).

shift_right_circular(L, [H|T]):-
    append(T, [H], L).



/*shift right has multiple rules*/
/*anything using append*/
