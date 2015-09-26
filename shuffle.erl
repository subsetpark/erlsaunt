-module(shuffle).
-export([shuffle/1]).

shuffle(List) ->
    shuffle(List, []).
shuffle([], Acc) ->
    Acc;
shuffle(List, Acc) ->
    % Randomly cut the cards
    {Leading, [H|T]} = lists:split(random:uniform(length(List)) - 1, List),
    % Take the top card from the bottom and put it on top of our shuffled pile,
    % And shuffle the remaining cards
    shuffle(Leading ++ T, [H|Acc]).
