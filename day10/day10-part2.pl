:- initialization(main).

% read_all_lines(+File, -Lines)
% Lines is a list of atoms, one per line in File.
read_all_lines(File, Lines) :-
    open(File, read, Stream),
    read_lines(Stream, Lines),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream), !.
read_lines(Stream, [LineAtom|Rest]) :-
    \+ at_end_of_stream(Stream),
    read_line(Stream, Chars),
    atom_chars(LineAtom, Chars),
    read_lines(Stream, Rest).

% read_line(+Stream, -Chars)
% (excludes trailing newline)
read_line(Stream, Chars) :-
    get_char(Stream, Char),
    ( Char = end_of_file   ->  Chars = []
    ; Char = '\n'          ->  Chars = []
    ; read_line(Stream, Rest), Chars = [Char | Rest]
    ).

% PARSING

parse_line(LineAtom, Machine) :-
    atom_codes(LineAtom, Codes),
    phrase(machine(Machine), Codes).

% machine(Grid, Buttons, Targets)
machine(machine(Grid, Buttons, Targets)) -->
    grid(Grid), ws, button_list(Buttons), ws, targets(Targets).

grid(GridChars) -->
    "[", grid_body(GridChars), "]".

grid_body([Bit|Bits]) -->
    [C],
    { ( C = 0'. -> Bit = 0
      ; C = 0'# -> Bit = 1
      )
    },
    !,
    grid_body(Bits).
grid_body([]) --> [].

button_list([B|Bs]) -->
    button(B),
    ws,
    ( button_list(Bs)
    ; { Bs = [] }
    ).

button(Positions) -->
    "(",
    int_list(Indices),
    { maplist(inc, Indices, Positions) },
    ")".

inc(X, Y) :-
    Y is X + 1.

targets(Targets) -->
    "{", int_list(Targets), "}".

% comma-separated list of integers
int_list([N|Ns]) -->
    integer(N),
    ( "," ->  int_list(Ns)
    ; { Ns = [] }
    ).

% signed/unsigned integer
integer(N) -->
    digits(Ds),
    { number_codes(N, Ds) }.

digits([D|Ds]) -->
    [D],
    { D >= 0'0, D =< 0'9 },
    digits_rest(Ds).

digits_rest([D|Ds]) -->
    [D],
    { D >= 0'0, D =< 0'9 },
    !,
    digits_rest(Ds).
digits_rest([]) --> [].

% optional whitespace (spaces or tabs)
ws --> [C], { C =:= 32 ; C =:= 9 }, !, ws.
ws --> [].

% SOLVE

part2(Machines, T) :-
    maplist(min_presses, Machines, MachineButtons),
    sum_list(MachineButtons, T).

% CONSTANTS - found by inspecting input
total_lights(10).
total_buttons(13).

% Solve via set of simultaneous equations:
%  - Equation per light (max 10)
%  - Free variable per button (max 13)
%  - Each variable range between 0 and max count inclusive (max 255)
min_presses(Machine, Buttons) :-
    Machine = machine(_, Buts, Counts),
    %format('Machine: ~w~n', [Machine]),

    maplist(buttons_to_flags, Buts, ButtonsAsFlags),
    normalise_buttons(ButtonsAsFlags, ButtonsNormalised),
    normalise_counts(Counts, CountsNormalised),

    % Presses of each buttons
    Vars = [A,B,C,D,E,F,G,H,I,J,K,L,M],
    % Constrain button counts
    max_list(CountsNormalised, MaxCount),
    fd_domain(Vars, 0, MaxCount),

    % Target button counts to overall counts
    total_lights(TotalLights),
    constrain_position(Vars, 1, TotalLights, ButtonsNormalised, CountsNormalised),

    % Sum all button presses
    A+B+C+D+E+F+G+H+I+J+K+L+M #= Buttons,

    % Search
    Options = [ variable_method(most_constrained),value_method(min) ],
    fd_minimize(fd_labeling(Vars, Options), Buttons).

constrain_position(_, Position, Total, _, _) :- Position > Total, !.
constrain_position([A,B,C,D,E,F,G,H,I,J,K,L,M], Position, Total, Buttons, Counts) :-
    nth(Position, Counts, Count),

    maplist(nth(Position), Buttons, ButtonFlags),

    % Extract each flag
    nth( 1, ButtonFlags, AFlag),
    nth( 2, ButtonFlags, BFlag),
    nth( 3, ButtonFlags, CFlag),
    nth( 4, ButtonFlags, DFlag),
    nth( 5, ButtonFlags, EFlag),
    nth( 6, ButtonFlags, FFlag),
    nth( 7, ButtonFlags, GFlag),
    nth( 8, ButtonFlags, HFlag),
    nth( 9, ButtonFlags, IFlag),
    nth(10, ButtonFlags, JFlag),
    nth(11, ButtonFlags, KFlag),
    nth(12, ButtonFlags, LFlag),
    nth(13, ButtonFlags, MFlag),

    % Express target as result of each button count
    A*AFlag + B*BFlag + C*CFlag + D*DFlag + E*EFlag + F*FFlag + G*GFlag + H*HFlag + I*IFlag + J*JFlag + K*KFlag + L*LFlag + M*MFlag #= Count,

    % Recurse
    Position1 is Position + 1,
    constrain_position([A,B,C,D,E,F,G,H,I,J,K,L,M], Position1, Total, Buttons, Counts).

buttons_to_flags(Indexes, Flags) :-
    total_lights(TotalLights),
    flags(1, TotalLights, Indexes, Flags).

flags(Position, Total, _, []) :- Position > Total, !.
flags(Position, Total, Indexes, [Flag|OtherFlags]) :-
    ( member(Position, Indexes) -> Flag = 1 ; Flag = 0 ),
    Position1 is Position + 1,
    flags(Position1, Total, Indexes, OtherFlags).

normalise_buttons(Buttons, Normalised) :-
    total_lights(TotalLights),
    right_pad([], TotalLights, 0, DefaultLights),

    total_buttons(TotalButtons),
    right_pad(Buttons, TotalButtons, DefaultLights, Normalised).

normalise_counts(Counts, Normalised) :-
    total_lights(TotalLights),
    right_pad(Counts, TotalLights, 0, Normalised).

right_pad(_, X, _, []) :- X =< 0, !.
right_pad([], X, Default, [Default|Rest]) :-
    X1 is X - 1,
    right_pad([], X1, Default, Rest).

right_pad([Item|Items], X, Default, [Item|Rest]) :-
    X1 is X - 1,
    right_pad(Items, X1, Default, Rest).

% ENTRY

main :-
    read_all_lines('/dev/stdin', Lines),
    maplist(parse_line, Lines, Machines),
    part2(Machines, Part2),
    format('Part2: ~w~n', [Part2]),
    halt(0).
