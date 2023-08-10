:-module('parent_eg_no_libs-prolog', []).
source_lang(en).
local_dict([is_the_parent_of, A, B], [x-x, y-y], [A, is, the, parent, of, B]).
local_dict([is_the_child_of, A, B], [y-y, x-x], [A, is, the, child, of, B]).
local_meta_dict([],[],[]).
prolog_le(verified).
is_the_parent_of(A, B) :-
    is_the_child_of(B, A).
example(null, []).
example(alice, [scenario([(is_the_child_of('Alice6', 'Bob'):-true)], true)]).
query(null, true).
query(q_ap, is_the_parent_of(_, 'Alice6')).
