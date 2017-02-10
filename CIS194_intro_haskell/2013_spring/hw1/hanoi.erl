-module(hanoi).

-type disk() :: integer().
-type peg() :: [disk()].
-type peg_name() :: a | b | c.
-type step() :: ab | ac | bc | ba | cb | ca.
-type steps() :: [step()].
-type state() :: {peg(), peg(), peg(), steps()}.

-export([init/1, solve/1]).


-spec init(integer()) -> state().
init(NumDisks) ->
    {lists:seq(1, NumDisks), [], [], []}.


-spec solve(state()) -> state().
solve(State) ->
    {A0, _, _, _} = State,
    {A, B, C, Steps} = move(length(A0), a, b, c, State),
    {A, B, C, lists:reverse(Steps)}.


-spec move(NumDisks :: integer(),
           From     :: peg_name(),
           To       :: peg_name(),
           Temp     :: peg_name(),
           State    :: state()) -> state().
move(0, _, _, _, State) -> State;
move(1, From, To, _, State) ->
    step(From, To, State);
move(2, From, To, Temp, State) ->
    State2 = step(From, Temp, State),
    State3 = step(From, To, State2),
    step(Temp, To, State3);
move(NumDisks, From, To, Temp, State) ->
    State2 = move(NumDisks-1, From, Temp, To, State),
    State3 = step(From, To, State2),
    move(NumDisks-1, Temp, To, From, State3).


-spec step(peg_name(), peg_name(), state()) -> state().
step(FromPeg, ToPeg, {A, B, C, Steps}) ->
    case {FromPeg, ToPeg} of
        {a, b} -> {F, T} = place(A, B), {F, T, C, [ab | Steps]};
        {a, c} -> {F, T} = place(A, C), {F, B, T, [ac | Steps]};
        {b, c} -> {F, T} = place(B, C), {A, F, T, [bc | Steps]};
        {b, a} -> {F, T} = place(B, A), {T, F, C, [ba | Steps]};
        {c, b} -> {F, T} = place(C, B), {A, T, F, [cb | Steps]};
        {c, a} -> {F, T} = place(C, A), {T, B, F, [ca | Steps]}
    end.


-spec place(FromPeg :: peg(), ToPeg :: peg()) -> {peg(), peg()}.
place([Top | Rest], []) -> {Rest, [Top]};
place([Top | Rest], [H | _] = ToPeg) when Top < H -> {Rest, [Top | ToPeg]}.
