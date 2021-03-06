-module(saunt).
-export_type([card/0]).
-export([test/0, hand_by_suit/1, longest/1, sort_hand/1, random_hand/0]).
-include("deck.hrl").

-type card() :: {rank(), suit()}.
-type rank() :: [6..14].
-type suit() :: spades | clubs | hearts | diamonds.
-type hand() :: [card()].
% Piquet consists of three stages:
%   - Exchange: The players take turns exchanging cards from the talon. Elder
%   may take up to 5 cards; Younger may take whatever's left (usually 3).
%   - Declaration: The players announce their Point, Sequence, and Sets.
%   - Trumps: The players play out their hands at trumps.

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

sort_hand(Hand) ->
    Key = fun({R, S}, {R2, S2}) ->
                  {S, R} =< {S2, R2}
          end,
    lists:sort(Key, Hand).

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
    play:test(),
    ok.

