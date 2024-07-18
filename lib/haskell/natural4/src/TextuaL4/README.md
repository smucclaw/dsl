# BNFC grammar for a textual format

This is **100% internal tool**, so that it's easier for us to generate tests quickly. This is not going into production, nor will it try to replace any of the other existing grammars.

## Example

```
EVERY tame ANY (Person, Animal) WHO Qualifies MEANS ALL(walks, ANY(eats, drinks), climbs) MUST ANY (sing, dance)

drinks MEANS consumes ANY (alcoholic, non-alcoholic) beverage
```

These translate into your usual NaturaL4 ASTs.

## How to run

Currently just like this:

```
echo "Drinks MEANS consumes ANY (alcoholic, non-alcoholic) beverage" | stack  run l4-bnfc-exe
```

TODO: workflow that just uses the grammar to run tests.


## How to develop

1. Edit the source file [../TextuaL4.cf](../TextuaL4.cf).
2. In ../ (dsl/lib/haskell/natural4/src), run the command `bnfc -m -p TextuaL4 TextuaL4.cf && make && rm TextuaL4/{Skel,Test}TextuaL.hs TextuaL4/DocTextual.txt TextuaL4/TestTextuaL Makefile`.
3. Commit all the generated files in TextuaL4 directory that were modified
4. Update [Transform.hs](./Transform.hs) according to what you changed in the grammar