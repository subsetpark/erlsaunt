-module(play).
-include("test.hrl").
-export([test/0, discard/3]).


-spec discard(Discards :: [saunt:card()], Hand :: [saunt:card()], Deck :: [saunt:card()]) ->
    {NewHand :: [saunt:card()], Rest :: [saunt:card()]}.

discard(Discards, Hand, Deck) ->
    DiscardCount = length(Discards),
    {TopCards, Rest} = lists:split(DiscardCount, Deck),
    {lists:flatten([TopCards|(Hand -- Discards)]), Rest}.

-define(DISCARD, [{6, clubs}, {7, clubs}]).
test() ->
    % discard test
    {NewHand, Rest} = discard(?DISCARD, ?SAMPLE_HAND, ?TALON),
    false = lists:any(fun(D) -> lists:member(D, NewHand) end, ?DISCARD),
    false = lists:any(fun(C) -> lists:member(C, Rest) end, NewHand),
    ok.



