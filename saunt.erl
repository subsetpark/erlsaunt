-module(saunt).
-export_type([hand/0, card/0]).
-export([test/0, hand_by_suit/1, longest/1, random_hand/0]).

-define(SUITS, [spades, clubs, hearts, diamonds]).
-define(RANKS, [7, 8, 9, 10, 11, 12, 13, 14]).
-define(DECK, [{Rank, Suit} || Rank <- ?RANKS, Suit <- ?SUITS]).

-type card() :: {rank(), suit()}.
-type rank() :: [6..14].
-type suit() :: spades | clubs | hearts | diamonds.

% Piquet consists of three stages:
%   - Exchange: The players take turns exchanging cards from the talon. Elder
%   may take up to 5 cards; Younger may take whatever's left (usually 3).
%   - Declaration: The players announce their Point, Sequence, and Sets.
%   - Trumps: The players play out their hands at trumps.

-spec compare_ranks(card(), card()) -> card() | even.
compare_ranks({LeftRank, _}, {RightRank, _}) when LeftRank > RightRank -> LeftRank;
compare_ranks({LeftRank, _}, {RightRank, _}) when RightRank > LeftRank -> RightRank;
compare_ranks(_, _) -> even.

-spec trump_hand(Lead :: card(), Follow :: card()) -> Winner :: card().
trump_hand({_, LeadSuit}=Lead, {_, FollowSuit}) when LeadSuit /= FollowSuit -> Lead;
trump_hand({LeadRank, _}=Lead, {FollowRank, _}) when LeadRank > FollowRank -> Lead;
trump_hand(_, Follow) -> Follow.

-type hand() :: [card()].



% Helper Functions
-spec hand_by_suit(hand()) -> [[card()]].
hand_by_suit(Hand) ->
    [[Card || {_, CardSuit}=Card <- Hand, CardSuit == Suit]
     || Suit <- ?SUITS ].

longest(Ls) ->
    Longer = fun(L1, L2) ->
                     if length(L2) > length(L1) -> L2;
                        true -> L1
                     end
             end,
    lists:foldl(Longer, [], Ls).

random_hand() ->
    Shuffled = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- ?DECK])],
    {Hand, _} = lists:split(12, Shuffled),
    Hand.

test() ->
    % Helper tests
    [a, b, c, d] = longest([[a], [a, b, c, d], [], [a, b,c ]]),
    % Module tests
    point:test(),
    sequence:test(),
    set:test(),
    ok.

