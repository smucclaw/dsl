# README

If you haven't already read Joe's specification "Denotational semantics of Relational Predicates", you should look at that first.

This README is meant to be a summary of some of the more nitty-gritty LE details that are important to keep in mind when implementing or understanding the L4 -> LE transpiler.

## Notes on Logical English

### NLAs / templates

From https://github.com/LogicalContracts/LogicalEnglish/blob/main/le_syntax.md

    * A template is a string of words and variable indicators separated by spaces. A word is string of letters or numbers. A variable indicator is a string of words prefixed by `*` and postfixed by `*`.
    * An instance of a template is an __expression__ obtained from the template by consistently replacing all the variable indicators by __terms__.
    * A __term__ is either
        * a variable, 
        * an expression (which include __constants__)  
        * or a compound (such as a list).

From "Logical English for Law and Education":
    * A __variable__ is a noun phrase ending with a common noun, such as “person” or “thing” and starting with a determiner such as “a”, “an” or “the”. 
        * The indefinite determiner, “a” or “an”, introduces the first occurrence of a variable in a sentence. The same noun phrase with the indefinite determiner replaced the definite determiner, “the”, represents all later occurrences of the same variable in the same sentence.
    * Any other string of words in the position of an argument place is a __constant__.


EGs:

An argument place can be filled by either a constant or a variable:
```
LE:     Alice likes a person if the person likes logic.
Prolog: likes(alice, A) :- likes(A, logic).
```


### Further reading

"Logical English for Law and Education" is quite readable / skimmable