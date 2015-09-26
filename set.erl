-module(set).
-behavior(declaration).
-import(saunt, [longest/1]).
-export([test/0, meld/1, meld_of/1, meld_score/1]).
-include("test.hrl").

-define(MELD_RANKS, [10, 11, 12, 13, 14]).

% Sets are any groups of 3 or more cards of the same rank (10 and up).
-type set() :: [saunt:card()].

-spec meld(saunt:hand()) -> [set()].
meld(Hand) ->
    HandRanks = [[Card || {CardRank, _}=Card <- Hand, CardRank == Rank]
                 || Rank <- ?MELD_RANKS],
    [Set || Set <- HandRanks, length(Set) >= 3].

meld_of(Hand) ->
    Sets = meld(Hand),
    length(longest(Sets)).

meld_score(Hand) ->
    Sets = meld(Hand),
    lists:sum([set_score(Set) || Set <- Sets]).
set_score(Set) ->
    case length(Set) of
        3 -> 3;
        4 -> 14
    end.


-define(SAMPLE_SET, [{10, clubs}, {10, diamonds}, {10, spades}, {11, clubs}, {11, diamonds}]).
-define(MULTIPLE_SETS, [{10, clubs}, {11, clubs}, {10, diamonds}, {11, diamonds}, {10, hearts}, {11, hearts}, {10, spades}, {12, spades}]).
-define(NO_SETS, [{10, clubs}]).
test() ->
    [[{10,clubs},{10,diamonds},{10,spades}]] = meld(?SAMPLE_SET),
    [[{10,clubs},{10,diamonds},{10,hearts},{10,spades}],[{11,clubs},{11,diamonds},{11,hearts}]] = meld(?MULTIPLE_SETS),
    4 = meld_of(?MULTIPLE_SETS),
    17 = meld_score(?MULTIPLE_SETS),
    % No sets
    0 = meld_of(?NO_SETS),
    0 = meld_score(?NO_SETS),
    ok.


