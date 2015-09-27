-module(play).
-include("test.hrl").
-include("deck.hrl").
-export([test/0, deal/0, discard/3]).

-define(HAND_SIZE, 12).

-spec deal() -> {Elder :: [saunt:card()], Younger :: [saunt:card()], Rest :: [saunt:card()]}.
deal() ->
    Shuffled = shuffle:shuffle(?DECK),
    Elder = lists:sublist(Shuffled, ?HAND_SIZE),
    Younger = lists:sublist(Shuffled, ?HAND_SIZE+1, ?HAND_SIZE),
    Rest = lists:sublist(Shuffled, ?HAND_SIZE*2+1, length(Shuffled) - ?HAND_SIZE*2),
    {Elder, Younger, Rest}.

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
    % deal test
    {E, Y, R} = deal(),
    {12, 12, 8} = {length(E), length(Y), length(R)},
    Dealt = lists:sort(lists:flatten([E, Y, R])),
    Dealt = lists:sort(?DECK),
    ok.



