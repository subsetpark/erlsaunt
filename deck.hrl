-define(SUITS, [spades, clubs, hearts, diamonds]).
-define(RANKS, [7, 8, 9, 10, 11, 12, 13, 14]).
-define(DECK, [{Rank, Suit} || Rank <- ?RANKS, Suit <- ?SUITS]).