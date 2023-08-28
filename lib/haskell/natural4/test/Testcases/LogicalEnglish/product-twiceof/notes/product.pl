:-module('product-prolog', []).
source_lang(en).
local_dict([is_twice_of, A, B], [x-x, y-y], [A, is, twice, of, B]).
local_meta_dict([],[],[]).
prolog_le(verified).
is_twice_of(A, B) :-
    between(0, 100, B),
    product_list([B, 2], A).
example(null, []).
query(null, true).
query(q, is_twice_of(_, _)).
