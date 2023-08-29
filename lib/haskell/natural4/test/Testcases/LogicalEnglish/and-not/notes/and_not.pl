:-module('and_not-prolog', []).
source_lang(en).
local_dict([is_an_aquatic_animal, A], [x-x], [A, is, an, aquatic, animal]).
local_dict([lives_on_land, A], [x-x], [A, lives, on, land]).
local_dict([lives_in_water, A], [x-x], [A, lives, in, water]).
local_meta_dict([],[],[]).
prolog_le(verified).
is_an_aquatic_animal(A) :-
    lives_in_water(A),
    not lives_on_land(A).
lives_in_water('Fish').
example(null, []).
query(null, true).
query(q, is_an_aquatic_animal(_)).
