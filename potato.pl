isEdiblePotato(Item) :- isPotato(Item), isEdible(Item).
isTastyFood(Item) :- isPotato(Item), isEdible(Item).

isPotato(bob).
isPotato(alice).

isEdible(bob).
isEdible(alice).

%% "forward-chaining"
%% if (x) { then Y }

%% "backward-chaining"
%% then Y :- if X

