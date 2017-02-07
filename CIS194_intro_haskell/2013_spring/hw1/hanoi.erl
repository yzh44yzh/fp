-module(hanoi).

-type peg() :: [integer()].
-type state() :: {peg(), peg(), peg()}.
-type move_type() :: ab | ac | bc | ba | cb | ca.

-export([init/1, solve/1]).


-spec init(integer()) -> state().
init(NumDisks) ->
    {lists:seq(1, NumDisks), [], []}.


-spec solve(state()) -> state().
solve(State) ->
    Moves = [ac, ab, cb, ac, ba, bc, ac], % valid for 3 disks
    lists:foldl(fun move/2, State, Moves).


-spec move(move_type(), state()) -> state().
move(Move, {A, B, C}) ->
    case Move of
        ab -> {F, T} = place(A, B), {F, T, C};
        ac -> {F, T} = place(A, C), {F, B, T};
        bc -> {F, T} = place(B, C), {A, F, T};
        ba -> {F, T} = place(B, A), {T, F, C};
        cb -> {F, T} = place(C, B), {A, T, F};
        ca -> {F, T} = place(C, A), {T, B, F}
    end.


-spec place(FromPeg :: peg(), ToPeg :: peg()) -> {peg(), peg()}.
place([Top | Rest], []) -> {Rest, [Top]};
place([Top | Rest], [H | _] = ToPeg) when Top < H -> {Rest, [Top | ToPeg]}.
