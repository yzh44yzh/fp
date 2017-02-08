-module(hanoi).

-type disk() :: integer().
-type peg() :: [disk()].
-type move_type() :: ab | ac | bc | ba | cb | ca.
-type state() :: {peg(), peg(), peg(), [move_type()]}.

-export([init/1, solve/1]).


-spec init(integer()) -> state().
init(NumDisks) ->
    {lists:seq(1, NumDisks), [], [], []}.


-spec solve(state()) -> state().
solve({[], _, [], _} = State) -> State; % solved
solve({[LastDisk], [], PedC, Moves}) -> solve({[], [LastDisk], PedC, [ab | Moves]});
solve(State) ->
    %% Move len(A)-1 disks from a to c
    %% Move lastA ab
    %% Move all C from c to b
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
