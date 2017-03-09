%% rock-paper-scissors game

-module(rps).
-export([game/2, tournament/2, play_two/3, play/1, all_strategies/0]).
-export([rock_strategy/1, echo_strategy/1, no_repeat_strategy/1,
         random_strategy/1, cycle_strategy/1,
         least_frequent_strategy/1, most_frequent_strategy/1,
         some_strategy/1, best_strategy/1
        ]).


-type move() :: rock | paper | scissors.
-type strategy() :: fun(([move()]) -> move()).

-define(WIN, 1).
-define(LOSE, -1).
-define(DRAW, 0).

-spec game(move(), move()) -> integer().
game(paper, rock) -> ?WIN;
game(rock, paper) -> ?LOSE;
game(scissors, paper) -> ?WIN;
game(paper, scissors) -> ?LOSE;
game(rock, scissors) -> ?WIN;
game(scissors, rock) -> ?LOSE;
game(Move, Move) -> ?DRAW.


-spec tournament([move()], [move()]) -> integer().
tournament(Moves1, Moves2) ->
    lists:sum(
      lists:zipwith(fun game/2, Moves1, Moves2)
      ).


%%
%% play one strategy against another, for N moves.
%%
-spec play_two(strategy(), strategy(), integer()) -> integer().
play_two(StrategyL, StrategyR, N) ->
    io:format("play ~p against ~p for ~p moves~n", [StrategyL, StrategyR, N]),
    play_two(StrategyL, StrategyR, [], [], N).


play_two(_, _, _, _, 0) -> 0;
play_two(StrategyL, StrategyR, PlaysL, PlaysR, N) ->
    MoveL = StrategyL(PlaysR),
    MoveR = StrategyR(PlaysL),
    Res = game(MoveL, MoveR),
    io:format("~p ~p ~p~n", [MoveL, MoveR, Res]),
    play_two(StrategyL, StrategyR, [MoveL | PlaysL], [MoveR | PlaysR], N-1).



%%
%% interactively play against a strategy, provided as argument.
%%
-spec play(strategy()) -> ok.
play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    Total = play(Strategy, [], 0),
    io:format("Total result ~p~n", [Total]).


-spec play(strategy(), [move()], integer()) -> integer().
play(Strategy, Moves, Total) ->
    {ok, P} = io:read("Play: "),
    Play = expand(P),
    case Play of
        stop ->
            io:format("Stopped~n"),
            Total;
        _    ->
            Result = game(Play, Strategy(Moves)),
            io:format("Result: ~p~n",[Result]),
            play(Strategy, [Play|Moves], Total+Result)
    end.



%% Strategies

rock_strategy(_) -> rock.


echo_strategy([]) -> random_strategy([]);
echo_strategy([Move | _]) -> Move.


%% your opponent never repeats herself
no_repeat_strategy([]) -> random_strategy([]);
no_repeat_strategy([Move|_]) ->
    case Move of
        rock -> scissors; % opponent will do scissors or paper
        paper -> rock; % opponent will do rock or scissors
        scissors -> paper % opponent will do rock or paper
    end.


random_strategy(_) ->
    case random:uniform(3) of
        1 -> rock;
        2 -> paper;
        3 -> scissors
    end.


%% cycles through the three choices in some order
cycle_strategy(Moves) ->
    case length(Moves) rem 3 of
        0 -> rock;
        1 -> paper;
        2 -> scissors
    end.


%% assuming that in the long run your opponent will play each choice equally
least_frequent_strategy(Moves) ->
    {R, P, S} = calc_moves(Moves),
    if
        R =< P andalso R =< S -> paper;
        P =< R andalso P =< S -> scissors;
        true -> rock
    end.


%% assuming that in the long run your opponent is going to play that choice more often than the others
most_frequent_strategy(Moves) ->
    {R, P, S} = calc_moves(Moves),
    if
        R >= P andalso R >= S -> paper;
        P >= R andalso P >= S -> scissors;
        true -> rock
    end.


%% Define a strategy that takes a list of strategies and each play chooses a random one to apply
-spec some_strategy([strategy()]) -> strategy().
some_strategy(Strategies) ->
    N = random:uniform(length(Strategies)),
    lists:nth(N, Strategies).


%% Define a strategy that takes a list of strategies
%% and each play chooses from the list the strategy
%% which gets the best result when played against the list of plays made so far.
-spec best_strategy([strategy()]) -> strategy().
best_strategy(Strategies) ->
    fun(Moves) ->
            Results = lists:map(fun(Strategy) ->
                                        Result = apply_strategy(Strategy, Moves),
                                        {Result, Strategy}
                                end, Strategies),
            Results2 = lists:sort(fun({R1,_}, {R2,_}) -> R1 > R2 end, Results),
            {_, BestStrategy} = hd(Results2),
            BestStrategy(Moves)
    end.


all_strategies() ->
    [
     fun rock_strategy/1,
     fun echo_strategy/1,
     fun no_repeat_strategy/1,
     fun random_strategy/1,
     fun cycle_strategy/1,
     fun least_frequent_strategy/1,
     fun most_frequent_strategy/1
     ].


%%
%% auxiliary functions
%%

%% transform shorthand atoms to expanded form
expand(r) -> rock;
expand(p) -> paper;
expand(s) -> scissors;
expand(X) -> X.


-spec calc_moves([move()]) -> {integer(), integer(), integer()}.
calc_moves(Moves) ->
    lists:foldl(
      fun
          (rock, {R, P, S}) -> {R+1, P, S};
          (paper, {R, P, S}) -> {R, P+1, S};
          (scissors, {R, P, S}) -> {R, P, S+1}
      end,
      {0,0,0}, Moves).


-spec apply_strategy(strategy(), [move()]) -> integer().
apply_strategy(_, []) -> 0;
apply_strategy(Strategy, [OppMove | Moves]) ->
    MyMove = Strategy(Moves),
    game(MyMove, OppMove) + apply_strategy(Strategy, Moves).
