:- initialization(main).

read_input(Patterns, Designs) :-
    open('input.txt', read, In),
    read_line_to_codes(In, PatternLineCodes),
    atom_codes(PatternAtom, PatternLineCodes),
    atomic_list_concat(PatternAtoms, ',', PatternAtom),
    maplist(strip_spaces_atom, PatternAtoms, Patterns),
    skip_blank_line(In),
    read_designs(In, Designs),
    close(In).

strip_spaces_atom(AtomIn, AtomOut) :-
    atom_chars(AtomIn, CharsIn),
    exclude(=( ' ' ), CharsIn, CharsNoSpaces),
    atom_chars(AtomOut, CharsNoSpaces).

skip_blank_line(In) :-
    read_line_to_codes(In, Codes),
    ( Codes = [] -> true ; skip_blank_line(In) ).

read_designs(In, Designs) :-
    read_line_to_codes(In, Codes),
    ( Codes = end_of_file ->
        Designs = []
    ; Codes = [] ->
        read_designs(In, Designs)
    ; atom_codes(Design, Codes),
      Designs = [Design|Rest],
      read_designs(In, Rest)
    ).

:- dynamic memo/2.
% memo(Design, Ways).

count_ways(Design, Patterns, Ways) :-
    ( memo(Design, Ways) ->
        true
    ; otherwise ->
        ( Design = '' ->
            Ways = 1
        ; findall(W, ( member(P, Patterns),
                        prefix(P, Design),
                        remove_prefix(Design, P, Rest),
                        count_ways(Rest, Patterns, W)
                      ),
                    Ws),
          sum_list(Ws, Ways0),
          ( Ws = [] -> Ways = 0 ; Ways = Ways0 ),
          asserta(memo(Design, Ways))
        )
    ).

prefix(P, S) :-
    atom_chars(P, PChars),
    atom_chars(S, SChars),
    append(PChars, _, SChars).

remove_prefix(S, P, Rest) :-
    atom_chars(S, SChars),
    atom_chars(P, PChars),
    append(PChars, RestChars, SChars),
    atom_chars(Rest, RestChars).

main :-
    read_input(Patterns, Designs),
    retractall(memo(_,_)), % Clear any old memoized results
    findall(Ways, (member(D, Designs), count_ways(D, Patterns, Ways)), WaysList),
    findall(1, (member(W, WaysList), W > 0), PossibleList),
    length(PossibleList, PossibleCount),
    sum_list(WaysList, TotalWays),
    % Output the results
    writeln(PossibleCount),
    writeln(TotalWays),
    halt.
