-module(point).
-behavior(declaration).
-import(saunt, [hand_by_suit/1, longest/1]).
-export([test/0, meld/1, meld_of/1, meld_score/1]).
-include("test.hrl").

% Point is determined by the length of the suit with the most cards in a hand.
-type point() :: [saunt:card()].

-spec meld(saunt:hand()) -> [point()].
meld(Hand) ->
    PointGroups = hand_by_suit(Hand),
    [longest(PointGroups)].

-spec meld_of(point()) -> non_neg_integer().
meld_of(Hand) -> meld_score(Hand).

-spec meld_score(point()) -> non_neg_integer().
meld_score(Hand) ->
    [Point] = meld(Hand),
    length(Point).

test() ->
    [[{6, clubs}, {7, clubs}, {9, clubs}, {10, clubs}]] = meld(?SAMPLE_HAND),
    4 = meld_of(?SAMPLE_HAND),
    4 = meld_score(?SAMPLE_HAND),
    ok.
