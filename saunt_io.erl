-module(saunt_io).
-compile([export_all]).
-export([main/0, get_discard/2]).

main() ->
    {Elder, Younger, Rest} = play:deal(),
    {Elder2, Rest2} = get_discard(Elder, Rest),
    io:get_line(hand_to_string(Elder2)),
    {Younger2, Rest3} = get_discard(Younger, Rest2),
    io:get_line(hand_to_string(Younger2)),
    io:format("Elder: ", []),
    declare_hand(Elder2),
    io:format("Younger: ", []),
    declare_hand(Younger2),
    {ElderScore, YoungerScore}=DeclarationScores = play:declare(Elder2, Younger2),
    io:format("~p~n", [DeclarationScores]),
    play_trumps({Elder2, Younger2}, {ElderScore, YoungerScore}, {0, 0}).

get_discard(Hand, Rest) ->
    io:format("Your hand:~n~s~n", [hand_to_string(Hand)]),
    DiscardNum = lists:min([length(Rest), 5]),
    Input = io:get_line("What would you like to discard? [" ++ integer_to_list(DiscardNum) ++ "] "),
    try parse_discard(Input, Hand) of
        Discards -> play:discard(Discards, Hand, Rest)
    catch
        not_in_hand ->
            io:format("Invalid entry.~n", []),
            get_discard(Hand, Rest);
        not_a_card ->
            io:format("I don't understand your input.", []),
            get_discard(Hand, Rest)
    end.
parse_discard(Input, Hand) ->
    Discards = string_to_hand(Input),
    lists:foreach(fun(Card) -> play:validate_membership(Card, Hand) end, Discards),
    Discards.

play_trumps({[], []}, Scores) -> Scores;
play_trumps({Lead, Follow}, {LeadScore, FollowScore} ) ->
    LeadCard = get_card(Lead, "Please Lead: ", none),
    FollowCard = get_card(Follow, "Please Follow: ", LeadCard),
    Winner = play:play_trump(LeadCard, FollowCard),
    {Lead2, Follow2} = {lists:delete(LeadCard, Lead), lists:delete(FollowCard, Follow)},
    FollowScore2 = if
                      Winner == follow -> FollowScore + 1;
                      true -> FollowScore
                  end,
    LeadScore2 = LeadScore + 1,
    if
        Winner == lead -> play_trumps({Lead2, Follow2}, {LeadScore2, FollowScore2});
        true -> play_trumps({Follow2, Lead2}, {FollowScore2, LeadScore2})
    end.

get_card(Hand, Prompt, Lead) ->
    Prompt2 = if
                  Lead /= none -> Prompt ++ "[" ++ card_to_string(Lead) ++ "] ";
                  true -> Prompt
              end,
    io:format("Your hand: ~p~n", [hand_to_string(Hand)]),
    Input = io:get_line(Prompt2),
    try parse_lead(Input, Hand, Lead) of
        Card -> Card
    catch
        not_in_hand ->
            io:format("You don't have that card.~n", []),
            get_card(Hand, Prompt, Lead);
        not_a_card ->
            io:format("I don't understand your input.", []),
            get_card(Hand, Prompt, Lead);
        must_follow_suit ->
            io:format("You must follow suit.~n", []),
            get_card(Hand, Prompt, Lead)
    end.
parse_lead(Input, Hand, Lead) ->
    [Card] = string_to_hand(Input),
    play:validate_membership(Card, Hand),
    play:validate_suit(Lead, Card, Hand),
    Card.

hand_to_string(Hand) ->
    string:join([card_to_string(Card) || Card <- saunt:sort_hand(Hand)], " ").
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
    [string_to_card(string:to_upper(Token)) || Token <- Tokens].
string_to_card(Token) ->
    RankStr = lists:droplast(Token),
    SuitStr = lists:last(Token),
    {string_to_rank(RankStr), string_to_suit(SuitStr)}.

string_to_suit($S) -> spades;
string_to_suit($D) -> diamonds;
string_to_suit($H) -> hearts;
string_to_suit($C) -> clubs;
string_to_suit(_) -> throw(not_a_card).

string_to_rank("A") -> 14;
string_to_rank("K") -> 13;
string_to_rank("Q") -> 12;
string_to_rank("J") -> 11;
string_to_rank(T) -> try
                         list_to_integer(T)
                     catch
                         error:_ -> throw(not_a_card)
                     end.

declare_hand(Hand) ->
    PointScore = point:meld_score(Hand),
    SequenceScore = sequence:meld_score(Hand),
    SetScore = set:meld_score(Hand),
    io:format("Point of ~p for ~p. Sequence of ~p for ~p. Set of ~p for ~p.~n",
              [point:meld_of(Hand), PointScore,
               sequence:meld_of(Hand), SequenceScore,
               set:meld_of(Hand), SetScore]).

-define(PRINT_HAND, [{7, diamonds}, {9, diamonds}, {8, hearts}, {10, hearts}, {13, hearts}]).
test() ->
    ?PRINT_HAND = string_to_hand("7D 9D 8H 10H KH\n"),
    "7D 9D 8H 10H KH" = hand_to_string(?PRINT_HAND),
    "10C" = card_to_string({10, clubs}),
    "AS" = card_to_string({14, spades}),
    {10, clubs} = string_to_card("10C"),
    {14, spades} = string_to_card("AS"),
    ok.

