-module(saunt_io).
-compile([export_all]).
-export([main/1, get_discard/2]).

main(State) ->
    {Elder, Younger, Rest} = play:deal(),
    {Elder2, Rest2} = get_discard(Elder, Rest),
    io:get_line(hand_to_string(Elder2)),
    {Younger2, Rest3} = get_discard(Younger, Rest2),
    io:get_line(hand_to_string(Younger2)),
    declare_hand(Elder2),
    declare_hand(Younger2).

get_discard(Hand, Rest) ->
    io:format("Your hand:~n~s~n", [hand_to_string(Hand)]),
    Input = io:get_line("What would you like to discard? [" ++ integer_to_list(length(Rest)) ++ "] "),
    Discards = string_to_hand(Input),
    case lists:all(fun(C) -> lists:member(C, Hand) end, Discards) of
        true -> play:discard(Discards, Hand, Rest);
        _ -> io:format("Invalid entry.~n", []),
             get_discard(Hand, Rest)
    end.

hand_to_string(Hand) ->
    string:join([card_to_string(Card)|| Card <- saunt:sort_hand(Hand)], " ").
card_to_string({Rank, Suit}) -> rank_to_string(Rank) ++ suit_to_string(Suit).

suit_to_string(spades) -> "S";
suit_to_string(diamonds) -> "D";
suit_to_string(hearts) -> "H";
suit_to_string(clubs) -> "C".

rank_to_string(14) -> "A";
rank_to_string(13) -> "K";
rank_to_string(12) -> "Q";
rank_to_string(11) -> "J";
rank_to_string(N) -> integer_to_list(N).

string_to_hand(HandString) ->
    Tokens = string:tokens(HandString, " \n"),
    [string_to_card(Token) || Token <- Tokens].
string_to_card(Token) ->
    RankStr = lists:droplast(Token),
    SuitStr = lists:last(Token),
    {string_to_rank(RankStr), string_to_suit(SuitStr)}.

string_to_suit($S) -> spades;
string_to_suit($D) -> diamonds;
string_to_suit($H) -> hearts;
string_to_suit($C) -> clubs.

string_to_rank("A") -> 14;
string_to_rank("K") -> 13;
string_to_rank("Q") -> 12;
string_to_rank("J") -> 11;
string_to_rank(T) -> list_to_integer(T).

declare_hand(Hand) ->
    PointScore = point:meld_score(Hand),
    SequenceScore = sequence:meld_score(Hand),
    SetScore = set:meld_score(Hand),
    io:format("Point of ~p for ~p. Sequence of ~p for ~p. Set of ~p for ~p.~n",
              [point:meld_of(Hand), PointScore,
               sequence:meld_of(Hand), SequenceScore,
               set:meld_of(Hand), SetScore]),
    io:format("Total score: ~p", [lists:sum([PointScore, SequenceScore, SetScore])]).

-define(PRINT_HAND, [{7, diamonds}, {9, diamonds}, {8, hearts}, {10, hearts}, {13, hearts}]).
test() ->
    ?PRINT_HAND = string_to_hand("7D 9D 8H 10H KH\n"),
    "7D 9D 8H 10H KH" = hand_to_string(?PRINT_HAND),
    "10C" = card_to_string({10, clubs}),
    "AS" = card_to_string({14, spades}),
    {10, clubs} = string_to_card("10C"),
    {14, spades} = string_to_card("AS"),
    ok.

