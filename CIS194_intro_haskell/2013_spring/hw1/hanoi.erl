-module(hanoi).

-type peg() :: [integer()].
-type state() :: {peg(), peg(), peg()}.
-type move_type() :: ab | ac | bc | ba | cb | ca.

-export([hanoi/1]).


-spec hanoi(integer()) -> state().
hanoi(NumDisks) ->
    State = {lists:seq(1, NumDisks), [], []},
    do_moves([ac, ab, cb, ac, ba, bc, ac], State).


-spec do_moves([move_type()], state()) -> state().
do_moves([], Res) -> Res;
do_moves([Move | Moves], State) ->
    io:format("~p ~p~n", [Move, State]), % TEMP
    do_moves(Moves, move(Move, State)).


-spec move(move_type(), state()) -> state().
move(ab, {A, B, C}) -> {F, T} = place(A, B), {F, T, C};
move(ac, {A, B, C}) -> {F, T} = place(A, C), {F, B, T};
move(bc, {A, B, C}) -> {F, T} = place(B, C), {A, F, T};
move(ba, {A, B, C}) -> {F, T} = place(B, A), {T, F, C};
move(cb, {A, B, C}) -> {F, T} = place(C, B), {A, T, F};
move(ca, {A, B, C}) -> {F, T} = place(C, A), {T, B, F}.


-spec place(FromPeg :: peg(), ToPeg :: peg()) -> {peg(), peg()}.
place([Top | Rest], []) -> {Rest, [Top]};
place([Top | Rest], [H | _] = ToPeg) when Top < H -> {Rest, [Top | ToPeg]}.
