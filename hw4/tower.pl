non_increasing([], []).
non_increasing([Hd|Tl], [Minval|Sublist]) :-
    non_increasing(Hd, Minval, 0, 0),
    non_increasing(Tl, Sublist).

non_increasing([], Minval, Temp, _) :-
    Minval #= Temp.

non_increasing([Hdd|Tll], Minval, Temp, NI) :-
    Hdd #> NI,
    Increment #= Temp + 1,
    non_increasing(Tll, Minval, Increment, Hdd).

non_increasing([Hdd|Tll], Minval, Temp, NI) :-
    Hdd #< NI,
    non_increasing(Tll, Minval, Temp, NI).
    
tower(N, T, C) :-
    len_row(T, N),
    len_col(T, N),
    within_domain(T, N),
    maplist(fd_all_different, T),
    transpose(T, O),
    maplist(fd_all_different, O),
    C = counts(Top, Bottom, Left, Right),
    len_row(Top, N),
    len_row(Bottom, N),
    len_row(Left, N),
    len_row(Right, N),
    non_increasing(T, Left),
    maplist(fd_labeling, T),
    reverse_2d(T, RT),
    reverse_2d(O, RO),
    non_increasing(RT, Right),
    non_increasing(O, Top),
    non_increasing(RO, Bottom).

reverse_2d(X, RX) :-
    maplist(reverse, X, RX).
	
len_row(T, N) :-
    length(T, N).

len_col([], _).
len_col([HD | TL], N) :-
    length(HD, N),
    len_col(TL, N).

within_domain([], _).
within_domain([HDDD | TLLL], NN) :-
    fd_domain(HDDD, 1, NN),
    within_domain(TLLL, NN).

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%PART2

within_domainP(N, Domain) :- 
    findall(X, between(1, N, X), Domain).
    
plain_tower(N, T, C) :-
    len_row(T, N),
    len_col(T, N),
    transpose(T, O),
    C = counts(Top, Bottom, Left, Right),
    len_row(Top, N),
    len_row(Bottom, N),
    len_row(Left, N),
    len_row(Right, N),
    non_increasingP(N, T, Left),
    reverse_2d(T, RT),
    non_increasingP(N, RT, Right),
    non_increasingP(N, O, Top),
    reverse_2d(O, RO),
    non_increasingP(N, RO, Bottom).

non_increasingP([], Minval, Temp, _) :-
    Minval is Temp.

non_increasingP([Hdd|Tll], Minval, Temp, NI) :-
    Hdd > NI,
    Increment is Temp + 1,
    non_increasingP(Tll, Minval, Increment, Hdd).

non_increasingP([Hdd|Tll], Minval, Temp, NI) :-
    Hdd < NI,
    non_increasingP(Tll, Minval, Temp, NI).

non_increasingP(_, [], _).
non_increasingP(N, [Hd|Tl], [Minval|Sublist]) :-
    within_domainP(N, Domain), 
    permutation(Domain, Hd),
    non_increasingP(Hd, Minval, 0, 0),
    non_increasingP(N, Tl, Sublist).

%PART 3

speedup(R) :-
    statistics(cpu_time, [_, _]),
    tower(5, _, counts([5,3,3,2,1],[1,2,3,3,2],[5,3,3,2,1],[1,2,2,3,2])),
    statistics(cpu_time, [_ , T]),
    statistics(cpu_time, [_, _]),
    plain_tower(5, _, counts([5,3,3,2,1],[1,2,3,3,2],[5,3,3,2,1],[1,2,2,3,2])),
    statistics(cpu_time, [_ , TT]),
    R is TT/T.


%PART 4

ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.
