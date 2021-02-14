
drule("34.1", SubRuleName, LP, shant, accept(execA, Business)) :-
    subRule(SubRuleName, LP, accept(execA, Business)).

drule("34.2", SubRuleName, LP, Deontic, accept(execA, Business)) :-
    %% subject to
    \+ drule("34.1", SubRuleName, LP, Deontic, accept(execA, Business)),
    whynot(LP, Deontic, Business).

whynot(_, may, smu) :- true. %% yihan may be dean

subRule(subRule_A, LP, accept(execA, Business)) :-
             detracts_from(Business, of(dignity, profession));
             incompat_with(Business, of(dignity, profession));
             derogates_from(Business, of(dignity, profession)).

subRule(subRule_B, LP, accept(execA, Business)) :-
     materially_interferes_with(Business, occ_of(LP, as(practising, lawyer))).

% facts: gambling detracts from the dignity of the legal profession.
detracts_from(gambling, of(dignity, profession)) :- true.
incompat_with(pizza, of(dignity, profession)) :- false.
derogates_from(pizza, of(dignity, profession)) :- false.

% facts: for some reason, Alice can keep her drinking under control,
materially_interferes_with(wine, occ_of(alice, as(practising, lawyer))) :- false.

% "does the wine business materially interfere with alice's ability to practise as a lawyer?"

% but Bob cannot.
materially_interferes_with(wine, occ_of(bob, as(practising, lawyer))) :- true.

     
