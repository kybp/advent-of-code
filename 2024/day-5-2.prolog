% X-Y is violated if it's a constraint and X appears after Y in L.
violations(L, [X-Y|Constraints], [X-Y|Violations]) :-
    after(X, Y, L),
    violations(L, Constraints, Violations).
violations(Updates, [X-Y|Constraints], Violations) :-
    \+ after(X, Y, Updates),
    violations(Updates, Constraints, Violations).
violations([], _, _).
violations(_, [], []).

after(X, Y, L) :-
    append(_, [Y|Tail], L),
    member(X, Tail).

middle(List, Middle) :-
    length(List, Length),
    HalfIndex is div(Length, 2),
    nth0(HalfIndex, List, Middle).

fix_violations(Update, Constraints, ReorderedUpdate) :-
    fix_violations_aux(Update, Constraints, [], ReorderedUpdate).

fix_violations_aux([], _, ReorderedUpdate, ReorderedUpdate).
fix_violations_aux([X|Rest], Constraints, Acc, ReorderedUpdate) :-
    insert_with_constraints(X, Acc, Constraints, AccWithX),
    fix_violations_aux(Rest, Constraints, AccWithX, ReorderedUpdate).

insert_with_constraints(X, Update, Constraints, UpdateWithX) :-
    insert(X, Update, UpdateWithX),
    violations(UpdateWithX, Constraints, []).

insert(X, [], [X]).
insert(X, [Y|Rest], [X,Y|Rest]).
insert(X, [Y|Ys], [Y|Rest]) :-
    insert(X, Ys, Rest).

middle_sum([Update|Updates], Constraints, Sum) :-
    violations(Update, Constraints, [_|_]),
    middle_sum(Updates, Constraints, RestSum),
    fix_violations(Update, Constraints, ReorderedUpdate),
    middle(ReorderedUpdate, Middle),
    Sum is RestSum + Middle.
middle_sum([_|Updates], Constraints, Sum) :-
    middle_sum(Updates, Constraints, Sum).
middle_sum([], _, 0).

parse_constraint(Line, X-Y) :-
    split_string(Line, "|", "", [XString, YString]),
    atom_number(XString, X),
    atom_number(YString, Y).

parse_update(Line, Update) :-
    split_string(Line, ",", "", UpdateStrings),
    maplist(atom_number, UpdateStrings, Update).

read_constraints(Stream, Constraints) :-
    read_line_to_string(Stream, Line),
    (  Line = ""
    -> Constraints = []
    ;  parse_constraint(Line, Constraint),
       read_constraints(Stream, RestConstraints),
       Constraints = [Constraint|RestConstraints]
    ).

read_updates(Stream, Updates) :-
    read_line_to_string(Stream, Line),
    (  Line = end_of_file
    -> Updates = []
    ;  parse_update(Line, Update),
       read_updates(Stream, RestUpdates),
       Updates = [Update|RestUpdates]
    ).

main :-
    open('input.txt', read, Stream),
    read_constraints(Stream, Constraints),
    read_updates(Stream, Updates),
    close(Stream),
    middle_sum(Updates, Constraints, MiddleSum),
    print(MiddleSum),
    nl.
