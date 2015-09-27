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

-define(DISCARD, [{8,clubs},{10,diamonds},{7,spades},{8,spades},{8,hearts}]).
-define(DISCARD_HAND, [{8,clubs}, {11,clubs}, {10,diamonds}, {11,diamonds},
                       {12,diamonds}, {13,diamonds}, {8,hearts}, {11,hearts},
                       {12,hearts}, {7,spades}, {8,spades}, {13,spades}]).
test() ->
    % discard test
    {Discards, Hand} = {?DISCARD, ?DISCARD_HAND},
    {5, 12} = {length(Discards), length(Hand)},
    {NewHand, Rest} = discard(Discards, Hand, ?TALON),
    false = lists:any(fun(D) -> lists:member(D, NewHand) end, Discards),
    false = lists:any(fun(C) -> lists:member(C, Rest) end, NewHand),
    % deal test
    {E, Y, R} = deal(),
    {12, 12, 8} = {length(E), length(Y), length(R)},
    Dealt = lists:sort(lists:flatten([E, Y, R])),
    Dealt = lists:sort(?DECK),
    ok.



