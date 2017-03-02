-module(file_index).
-export([index/1, get_file_contents/1, show_file_contents/1]).

%%% NOTES:

%% I have used maps() data structure and high order functions like map and fold.
%% I know they are not described in this course yet, but not using them lead to very verbose code :)

%% I remove short words and common words, but didn't do
%% "Normalising so that common endings, plurals etc. identified"

%% The task says index should be a list of entries:
%% { "foo" , [{3,5},{7,7},{11,13}] }
%% I belive index should be map(), and one-number ranges, like {7,7}
%% should be replaced with just that one number.


%%% Data Types
-type line() :: string().
-type word() :: string().
-type range() :: {integer(), integer()}.
-type word_index() :: {word(), [range()]}.
-type index() :: [word_index()].


%%% Module API

-spec index([line()]) -> index().
index(Lines) ->
    NumeratedLines = numerate_lines(Lines),
    AllWords = lists:foldl(fun index_line/2, maps:new(), NumeratedLines),
    GoodWords = maps:filter(fun(Word, _LineNumber) -> keep_word(Word) end, AllWords),
    NormalizedWords = maps:map(fun(_Word, LineNumbers) -> normalize_line_numbers(LineNumbers) end, GoodWords),
    lists:sort(maps:to_list(NormalizedWords)).


% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.
get_file_contents(Name) ->
    {ok, Bin} = file:read_file(Name),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    lists:map(fun binary_to_list/1, Lines).


% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.
show_file_contents(Lines) ->
    lists:map(fun(L) -> io:format("~s~n", [L]) end, Lines).



%%% Inner functions

-spec numerate_lines([line()]) -> [{integer(), line()}].
numerate_lines(Lines) ->
    Numbers = lists:seq(1, length(Lines)),
    lists:zip(Numbers, Lines).


-spec index_line({integer(), string()}, map()) -> map().
index_line({LineNumber, Line}, Dict) ->
    Words = string:tokens(Line, " ,.\\\"\'()[]/`!?;:"),
    lists:foldl(
      fun(Word, Acc) ->
              LWord = string:to_lower(Word),
              case maps:find(LWord, Acc) of
                  {ok, LineNumbers} -> Acc#{LWord := [LineNumber | LineNumbers]};
                  error -> Acc#{LWord => [LineNumber]}
              end
      end, Dict, Words).


%% Turns list of line numbers to list of ranges.
%% For example, turns [27,25,24,22,14,14,13] to [{13,14}, {22,22}, {24,25}, {27,27}].
%% We know LineNumbers are sorted in descending order as they are result of fold.
-spec normalize_line_numbers([integer()]) -> [range()].
normalize_line_numbers(LineNumbers) ->
    [H | T] = LineNumbers,
    LastRange = {H, H},
    {FirstRange, AllRanges} =
        lists:foldl(
          fun(Number, {CurrRange, Ranges}) ->
                  {From, To} = CurrRange,
                  if
                      Number == From -> {CurrRange, Ranges};
                      Number == (From - 1) -> {{Number, To}, Ranges};
                      Number < (From - 1) -> {{Number, Number}, [CurrRange | Ranges]}
                      %% we will crash if Number > From, but it should not happend
                  end
          end,
          {LastRange, []},
          T),
    [FirstRange | AllRanges].


-define(COMMON_WORDS, ["all", "and", "any", "are", "but", "can", "for", "not", "the"]).

-spec keep_word(word()) -> boolean().
keep_word(Word) when length(Word) < 3 -> false;
keep_word(Word) -> not lists:member(Word, ?COMMON_WORDS).
