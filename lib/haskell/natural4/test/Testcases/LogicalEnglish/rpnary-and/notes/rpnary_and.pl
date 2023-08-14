:-module('rpnary_and-prolog', []).
source_lang(en).
local_dict(['is_mercury_-_vapor', A], [x-x], [A, is, mercury, -, vapor]).
local_dict(['is_low_-_pressure', A], [x-x], [A, is, low, -, pressure]).
local_dict(['is_gas_-_discharge', A], [x-x], [A, is, gas, -, discharge]).
local_dict([is_a_fluorescent_lamp, A], [x-x], [A, is, a, fluorescent, lamp]).
local_meta_dict([],[],[]).
prolog_le(verified).
is_a_fluorescent_lamp(A) :-
    'is_low_-_pressure'(A),
    'is_mercury_-_vapor'(A),
    'is_gas_-_discharge'(A).
example(null, []).
example(lamp, [scenario([('is_low_-_pressure'('Lamp'):-true), ('is_mercury_-_vapor'('Lamp'):-true), ('is_gas_-_discharge'('Lamp'):-true)], true)]).
query(null, true).
query(q, is_a_fluorescent_lamp(_)).
