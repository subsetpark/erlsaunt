-module(declaration).
-type meld() :: [saunt:card()].

-callback meld(Hand :: saunt:hand()) -> Meld :: meld().
-callback meld_of(Hand :: saunt:hand())  -> non_neg_integer().
-callback meld_score(Hand :: saunt:hand()) -> non_neg_integer().
