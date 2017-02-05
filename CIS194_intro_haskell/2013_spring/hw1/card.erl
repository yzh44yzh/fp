-module(card).

-export([validate/1]).

%% validate(4012888888881881) is true

-spec validate(integer()) -> boolean().
validate(CardNum) ->
    Digits = to_digit_list(CardNum, []),
    Digits2 = double_odd_positions(Digits, odd, []),
    Sum = sum_digits(Digits2),
    Sum rem 10 == 0.


-spec to_digit_list(integer(), [integer()]) -> [integer()].
to_digit_list(Num, Acc) when Num < 10 -> [Num | Acc];
to_digit_list(Num, Acc) ->
    to_digit_list(Num div 10, [(Num rem 10) | Acc]).


-spec double_odd_positions([integer()], atom(), [integer()]) -> [integer()].
double_odd_positions([], _, Acc) -> lists:reverse(Acc);
double_odd_positions([H | T], odd, Acc) ->
    double_odd_positions(T, even, [(H * 2) | Acc]);
double_odd_positions([H | T], even, Acc) ->
    double_odd_positions(T, odd, [H | Acc]).


-spec sum_digits([integer()]) -> integer().
sum_digits(Digits) ->
    lists:foldl(
      fun
          (N, A) when N > 10 -> sum_digits(to_digit_list(N, [])) + A;
          (N, A) -> N + A
      end,
      0, Digits).
