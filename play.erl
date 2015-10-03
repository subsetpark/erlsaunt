-module(play).
-include("test.hrl").
-include("deck.hrl").
-export([test/0, deal/0, discard/3, declare/2, play_trump/2, validate_suit/3, validate_membership/2]).

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

-spec declare(Elder :: [saunt:card()], Younger :: [saunt:card()]) -> {ElderScore :: non_neg_integer(), YoungerScore :: non_neg_integer()}.
declare(Elder, Younger) ->
    {ElderPoint, YoungerPoint} = get_scores(point, Elder, Younger),
    {ElderSequence, YoungerSequence} = get_scores(sequence, Elder, Younger),
    {ElderSet, YoungerSet} = get_scores(set, Elder, Younger),
    {lists:sum([ElderPoint, ElderSequence, ElderSet]),
     lists:sum([YoungerPoint, YoungerSequence, YoungerSet])}.

get_scores(Module, Elder, Younger) ->
    ElderAnnounce = Module:meld_of(Elder),
    YoungerAnnounce = Module:meld_of(Younger),
    if
        ElderAnnounce =:= YoungerAnnounce -> {0, 0};
        ElderAnnounce > YoungerAnnounce -> {Module:meld_score(Elder), 0};
        ElderAnnounce < YoungerAnnounce -> {0, Module:meld_score(Younger)}
    end.

-spec play_trump(Lead :: saunt:card(), Follow :: saunt:card()) -> Winner :: atom().
play_trump({_, LeadSuit}, {_, FollowSuit}) when LeadSuit /= FollowSuit -> lead;
play_trump({LeadRank, _}, {FollowRank, _}) when LeadRank > FollowRank -> lead;
play_trump(_, _) -> follow.


validate_membership(Card, Hand) ->
    case lists:member(Card, Hand) of
        false -> throw(not_in_hand);
        _ -> ok
    end.

validate_suit(none, _, _) -> ok;
validate_suit({_, LeadSuit}, {_, CardSuit}, _) when LeadSuit == CardSuit ->
    ok;
validate_suit({_,LeadSuit}, _, Hand) ->
    case length([1 || {_, S} <- Hand, S == LeadSuit]) of
        0 -> ok;
        _ -> throw(must_follow_suit)
    end.

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
    % member validation test
    ok = validate_membership({8,clubs}, ?DISCARD_HAND),
    caught = try
                 validate_membership({14, diamonds}, ?DISCARD_HAND)
             catch
                 not_in_hand -> caught
             end,
    % suit validation test
    ok = validate_suit(none, {8, clubs}, ?DISCARD_HAND),
    ok = validate_suit({11, clubs}, {8, clubs}, ?DISCARD_HAND),
    caught = try
                 validate_suit({8, clubs}, {13, diamonds}, ?DISCARD_HAND)
             catch
                 must_follow_suit -> caught
             end,
    ok.



