member(X, [X|_]).
member(X, [_|T]) :- 
	member(X, T).

append([], L, L).
append([H|T], L, [H|R]) :- 
	append(T, L, R).

insert(X, L, [X|L]).
insert(X, [H|T], [H|R]) :-
    insert(X, T, R).

permute([], []).

permute([H|T], P) :-
    permute(T, PT),
    insert(H, PT, P).
    
    
countres(_, _, [], 0).
countres(Day, Time, [res(Day, Time, _, _)|T], N) :-
    countres(Day, Time, T, N1),
    N is N1 + 1.
	
countres(Day, Time, [res(D, Tm, _, _)|T], N) :-
    (Day \= D ; Time \= Tm),
    countres(Day, Time, T, N).


check_staff(Day, Time, Reservations) :-
    staff(Day, Staffno),
    countres(Day, Time, Reservations, Count),
    Count =< Staffno.

tablecapacity(Name, [t(Name, Cap)|_], Cap).
tablecapacity(Name, [_|T], Cap) :-
	tablecapacity(Name, T, Cap).


validtablehelp(Group, TableName) :-
    group(Group, Count, _),
    tables(Tablelist),
    tablecapacity(TableName, Tablelist, Cap),
    Cap >= Count.

validtables([]).
validtables([res(_, _, Group, Table)|T]) :-
    validtablehelp(Group, Table),
    validtables(T).


notableconflict([]).
notableconflict([res(D, T, _, Table)|R]) :-
    \+ member(res(D, T, _, Table), R),
    notableconflict(R).

validtimes([]).
validtimes([res(_, Time, Group, _)|T]) :-
    group(Group, _, Time),
    validtimes(T).


validstaff([], _).

validstaff([Day|Rest], Schedule) :-
    check_staff(Day, morning, Schedule),
    check_staff(Day, evening, Schedule),
    validstaff(Rest, Schedule).
    
assign([], _, []).
assign([group(G, _, Time)|Glist], Days, [res(Day, Time, G, Table)|R]) :-
    member(Day, Days),
    tables(TableList),
    member(t(Table, Capacity), TableList),
    group(G, Count, _),
    Capacity >= Count,
    assign(Glist, Days, R),
    notableconflict([res(Day, Time, G, Table)|R]).


collectgroups(L) :-
    findall(group(G,C,T), group(G,C,T), L).


schedule_all_reservations(Days, Schedule) :-
    collectgroups(Glist),
    assign(Glist, Days, TempSchedule),
    validstaff(Days, TempSchedule),
    permute(TempSchedule, Schedule).


dishingredients([], []).
dishingredients([D|T], R) :-
    recipe(D, L1),
    dishingredients(T, L2),
    append(L1, L2, R).


group_ingredients(Group, Ingredients) :-
    order(Group, Dishes),
    dishingredients(Dishes, Ingredients).


sepgroupfromrest(_, [], [], []).
sepgroupfromrest(Day, [res(Day, _, G, _)|T], Rest, [G|Glist]) :-
    sepgroupfromrest(Day, T, Rest, Glist).
sepgroupfromrest(Day, [res(D2, Tm, G, Tb)|T], [res(D2, Tm, G, Tb)|Rest], Glist) :-
    Day \= D2,
    sepgroupfromrest(Day, T, Rest, Glist).


ingredientsgrouplist([], []).
ingredientsgrouplist([G|T], R) :-
    group_ingredients(G, L1),
    ingredientsgrouplist(T, L2),
    append(L1, L2, R).

needed_ingredients([], []).
needed_ingredients(All, [(Day, L)|R]) :-
    All = [res(Day, _, _, _)|_],
    sepgroupfromrest(Day, All, Rest, Groups),
    ingredientsgrouplist(Groups, L),
    needed_ingredients(Rest, R).


write_reservations_to_csv(File, Schedule) :-
    open(File, write, Stream),
    write(Stream, 'Day, Month, Time, Group, Table'),nl(Stream),
    write_reslist(Stream, Schedule),
    close(Stream).


write_reslist(_, []).
write_reslist(S, [res(day(D,M), T, G, Tb)|R]) :-
    write(S, D), write(S, ','),
    write(S, M), write(S, ','),
    write(S, T), write(S, ','),
    write(S, G), write(S, ','),
    write(S, Tb), nl(S),
    write_reslist(S, R).


write_ingredients_to_csv(File, AllIngredients) :-
    open(File, write, Stream),
    write(Stream, 'Day, Month, Ingredients'), nl(Stream),
    write_inglist(Stream, AllIngredients),
    close(Stream).



write_inglist(_, []).
write_inglist(S, [(day(D,M), L)|R]) :-
    write(S, D), write(S, ','),
    write(S, M), write(S, ','),
    write_ingvals(S, L),
	nl(S),
    write_inglist(S, R).


write_ingvals(_, []).
write_ingvals(S, [X]) :-
	write(S, X).
write_ingvals(S, [H|T]) :-
    write(S, H), write(S, ';'),
    write_ingvals(S, T).
