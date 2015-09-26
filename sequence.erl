-module(sequence).
-behavior(declaration).
-import(saunt, [hand_by_suit/1, longest/1]).
-export([test/0, meld/1, meld_of/1, meld_score/1]).
-include("test.hrl").

-type sequence() :: [saunt:card()].
% Sequences are runs of 3 or more cards within a single suit.
-spec meld(saunt:hand()) -> [sequence()].
meld(Hand) ->
    Suits = [suit_sequences(Suit) || Suit <- hand_by_suit(Hand)],
    [Sequence || Sequences <- Suits, Sequence <- Sequences].

-spec meld_of(saunt:hand()) -> non_neg_integer().
meld_of(Hand) ->
    Sequences = meld(Hand),
    length(longest(Sequences)).

-spec meld_score(saunt:hand()) -> non_neg_integer().
meld_score(Hand) ->
    Sequences = meld(Hand),
    lists:sum([sequence_score(Sequence) || Sequence <- Sequences]).
sequence_score(Sequence) ->
    case length(Sequence) of
        3 -> 3;
        4 -> 4;
        N -> N + 10
    end.

build_sequence(Hand) -> build_sequence(Hand, []).
build_sequence([{HRank, _}=H, {H2Rank, _}=H2|Hand], Sequence) when H2Rank - HRank == 1 ->
    build_sequence([H2|Hand], [H|Sequence]);
build_sequence([H|_], Sequence) ->
    lists:reverse([H|Sequence]);
build_sequence([], Sequence) ->
    lists:reverse(Sequence).

suit_sequences(Suit) ->
    suit_sequences(lists:sort(Suit), []).
suit_sequences([], Sequences) ->
    lists:sort([Sequence || Sequence <- Sequences, length(Sequence) >= 3]);
suit_sequences(Suit, Sequences) ->
    Sequence = build_sequence(Suit),
    suit_sequences(Suit -- Sequence, [Sequence|Sequences]).

-define(NO_SEQUENCES, [{10, clubs}]).
test() ->
    % A sequence is any contiguous run of cards within a suit of length 3 or more.
    [{6,clubs},{7,clubs}, {8, clubs}] = build_sequence(?SAMPLE_SUIT),
    % All sequences in a suit can be counted for points, though any card may
    % appear in only one sequence.
    [[{6,clubs}, {7,clubs}, {8, clubs}], [{10, clubs}, {11, clubs}, {12, clubs}]] = suit_sequences(?MANY_SEQUENCES),
    [[{6,clubs}, {7,clubs}, {8,clubs}, {9,clubs}, {10, clubs}]] = suit_sequences(?LONG_SEQUENCE),
    % Every distinct sequence in a hand counts towards its sequence score.
    [[{6,clubs},{7,clubs},{8,clubs}]] = meld(?SAMPLE_SUIT),
    [[{11, spades}, {12, spades}, {13, spades}]] = meld(?SAMPLE_HAND),
    [[{7,clubs},{8,clubs},{9,clubs}],[{7,diamonds},{8,diamonds},{9,diamonds}]] = meld(?MULTIPLE_SEQUENCES),
    % No sequences
    0 = meld_of(?NO_SEQUENCES),
    0 = meld_score(?NO_SEQUENCES),
    ok.
