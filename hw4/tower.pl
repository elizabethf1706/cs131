
% NTOWER functions

% base case empty ematrix
transpose([], []).
% base case for matrix with empty rows.
transpose([[]|_], []).

transpose(Matrix, Transposed) :-
    maplist(head_tail, Matrix, Heads, Tails),
    transpose(Tails, TransposedTail),
    append([Heads], TransposedTail, Transposed).

head_tail([H|T], H, T   ).

different_constraints(_, []).
different_constraints(N, [Head|Tail]) :-
    fd_all_different(Head),
    different_constraints(N, Tail).

length_constraints(_, []).
length_constraints(N, [Head|Tail]) :-
    length(Head, N),
    length_constraints(N, Tail).

domain_constraints(_, []).
domain_constraints(N, [Head|Tail]) :-
    fd_domain(Head, 1, N),
    domain_constraints(N, Tail).


 % ====================================
ntower(N, T, C) :-
    % basic integer checking stuff
    integer(N), N >= 0,
    length(T, N),
    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),

    length_constraints(N,T),
    different_constraints(N,T),
    domain_constraints(N,T),
  
    transpose(T, TranT),
    length_constraints(N,TranT),
    different_constraints(N,TranT),
    domain_constraints(N,TranT),

    maplist(fd_labeling, T),
    C = counts(Top, Bottom, Left, Right),
    
    % top & bottom use transpose
    count_all(TranT, Top),
    maplist(reverse, TranT, TranTReverse),
    count_all(TranTReverse, Bottom),

    % left & right use original
    count_all(T, Left),
    maplist(reverse, T, ReverseT),
    count_all(ReverseT, Right).

% ==================================================

count_all([], []).
count_all([Row|Rest], [Count|Counts]) :-
    count_tallest(Row, 0, Count),
    count_all(Rest, Counts).

 
count_tallest([], Tallest, 0).
count_tallest([Height|Tail], Tallest, Count) :-
    (Tallest > Height -> count_tallest(Tail, Tallest, Count) ; 
 
    count_tallest(Tail, Height, NewCount), Count is NewCount + 1).

% ===== tower plain !
plain_ntower(N, T, C) :-
    % basic integer checking stuff
    integer(N), N >= 0,

    length(Top, N),
    length(Bottom, N),
    length(Left, N),
    length(Right, N),
    transpose(T, TranT),
 
    specify_domain(N, Domain), 

    fill(T, TranT, Domain), 
 
    length_constraints(N,TranT),
    length_constraints(N, T),

    different_constraints_plain(N, T),
    different_constraints_plain(N, TranT),

    C = counts(Top, Bottom, Left, Right),
 
    count_all(TranT, Top),
    maplist(reverse, TranT, TranTReverse),
    count_all(TranTReverse, Bottom),
 
    count_all(T, Left),
    maplist(reverse, T, ReverseT),
    count_all(ReverseT, Right).  

different_constraints_plain(_,[]).
different_constraints_plain(N, [Head | Tail]) :-
    all_diff(Head),        % Ensure the unique elements
    maplist(between(1, N), Head),
    different_constraints_plain(N, Tail).

%  good
all_diff([]).
all_diff([Element|Tail]) :-
    \+ member(Element, Tail),
    all_diff(Tail).

specify_domain(N, Domain) :- 
    generate_domain(1, N, Domain).

generate_domain(N, N, [N]).
generate_domain(I, N, [I|Rest]) :-
    I < N,
    I1 is I + 1,
    generate_domain(I1, N, Rest).

fill([], [], _).
fill([Head | Tail], [TransposeHead | TransposeTail], Domain):-
    fill_row(Head, TransposeHead,Domain),
    fill(Tail, TransposeTail, Domain).
fill_row(Row, TRow, Domain) :-
    permutation(Domain, Row),
    permutation(Domain, TRow).
% part 3 =============================================
ambiguous(N, C, T1, T2) :-
    ntower(N,T2,C),
    ntower(N,T1,C),
    T1 \= T2.


speedup(FloatingRatio) :-
    % Measure time for ntower
    statistics(cpu_time, [Start1|_]),
    ntower(5, T1, counts([2,3,2,1,4], [3,1,3,3,2], [4,1,2,5,2], [2,4,2,1,2])),
    statistics(cpu_time, [End1|_]),
    Time1 is End1 - Start1,
    
    % Measure time for plain_ntower
    statistics(cpu_time, [Start2|_]),
    plain_ntower(5, T2, counts([2,3,2,1,4], [3,1,3,3,2], [4,1,2,5,2], [2,4,2,1,2])),
    statistics(cpu_time, [End2|_]),
    Time2 is End2 - Start2,

    % Calculate the ratio of the times
    FloatingRatio is Time2 / Time1.
