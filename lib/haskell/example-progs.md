# Discuss and look at various hornlike rules

* What's the rule-name for?
* How is the rule-name determined?

## Functions and constants

```
GIVEN d DECIDE g d IS y
WHERE
    y's book IS green IF d > 0;
    y's book IS red OTHERWISE
```

Simple function
Corresponds to the function:

> g :: Int -> { book: 'red | 'green }
> g d = if d > 0 then { book: green} else { book: red }

Or the simala function:

> g = fun (d) => if d > 0 then { book = 'green } else { book = 'red }

**Question**

Is this function equivalent? Or is this a constant?

```
GIVEN d
GIVETH y
DECIDE
    y's book IS green IF d > 0;
    y's book's size IS g z OTHERWISE
WHERE
    z IS 5
    g d IS 5


GIVETH d
DECIDE d IS 5;
```

The generic mathlang translation seems to turn this one into:

```
[ param d, param y.green, param y.red
, y = true
, if d > 0 then y.book = green
, if true then y.book = red
]
```

In short, this shouldn't work, as this rule doesn't have a name and is thus unusable.

Or is `y` a global variable? It can't be in this instance, but what about

```
GIVETH y
DECIDE y
WHERE
    y's book IS green IF d > 0;
    y's book IS red OTHERWISE
```

Would that be supposed to work?

**ANSWER**

As `d` can be inferred as a global variable, we are thinking of a new keyword, such as `WITH` or `ASSUME`.

```
ASSUME d
GIVETH y
WHERE
    y's book IS 'green IF d > 0;
    y's book IS 'red OTHERWISE
```

Translates to:

> let d = ... : Int
> in let y = if d > 0 then { book: 'green } else { book: 'red }

### Declarations

**Valid definition**

Prefix (1 arg)

```
GIVEN x
DECIDE f x IS SUM(x, 3)
```

Postfix

```
GIVEN x
DECIDE x f IS SUM(x, 3)
```

Prefix (2 args)

```
GIVEN x, y
DECIDE f x y IS SUM(x, 3)
```

Infix (2 args)

```
GIVEN x, y
DECIDE x f y IS SUM(x, 3)
```

**Invalid definition**

`y` is bound in where, that's not allowed.

```
GIVEN x
DECIDE f x y IS SUM(x, y)
WHERE y IS 5
```

`d` is a global variable, not allowed in function definition

```
GIVEN x
DECIDE f x d IS SUM(x, d)
```


## GIVEN and GIVETHS

```
GIVETH x, y
DECIDE
    x IS 5;
    y IS 7
```

Is this two constants, or what is this supposed to mean?

`GIVEN`s seem to be generally rather optional, are they simply variable declarations?

**ANSWER**

These are indeed two constants.

## Name shadowing

**In bnfc, rules are not allowed in WHERE**

```
GIVEN x IS A NUMBER
DECIDE g x IS f x
WHERE
    GIVEN x DECIDE f x IS SUM(x, x)
```

## Functions

Functions are of the form

```
GIVEN x [AS A type], y [IS ONE OF ...], ...
DECIDE f x y ... IS <body>
```

As a concrete example

```
GIVEN x
DECIDE f x IS 5
```

`f` is the function name, `x` the parameter and `5` is the function body.
So the function `f` called with any parameter produces the result `5`.

All parameters *must* be `GIVEN`s and occur on the lhs of `IS`.
The name of the function, which must be free can occur for single parameters in
either `Prefix` or `Postfix`, and for two parameters in `Prefix` or `Infix` notation.
For anything else, the function must be in `Prefix` notation.

E.g.

* `f x`: Prefix
* `x f`: Postfix
* `x f y`: Infix
* `f x y`: Prefix
* `f a b c d ...`: Prefix.

### Recursion

Possible example:

```
GIVEN x
DECIDE
    f x IS SUM ( f MINUS ( x , 1 ) , f MINUS ( x , 1 ) ) IF x > 0;
    f x IS 0 OTHERWISE
```

However, this fail to parse and the equivalent csv syntax also feels like it would not support this.

## Inline Enums for GIVEN and GIVETH

```
GIVETH x IS ONE OF foo, bar, baz DECIDE x IS foo
```

Assign that x is some output

> x :: foo | bar | baz
> x = foo

```
GIVEN
    y IS A NUMBER
GIVETH
    x IS ONE OF foo, bar, baz
DECIDE
    x IS foo IF y > 5;
    x IS bar IF y < 0;
    x IS baz OTHERWISE
```

Assign a value to the variable x based on the value of 'y'
Should translate to:

> x = \y -> if y > 5 then 'foo else if y < 0 then 'bar else 'baz

```
GIVEN x IS ONE OF foo, bar, baz
DECIDE
    foo x IS 5 IF x IS foo;
    foo x IS 10 OTHERWISE
```

Assign that x is some output

    foo :: foo | bar | baz -> Number
    foo x = if x == foo
           then 5
           else 10

```
GIVEN x IS ONE OF foo, bar, baz;
      y IS ONE OF foo, bar, baz
DECIDE
    foo x IS 5 IF x IS foo AND y IS foo;
    foo x IS 10 OTHERWISE
```

Are x and y referring to the same type?
What about:

```
GIVEN x IS ONE OF foo, bar, baz
      y IS ONE OF foo, bar, foo baz
```

Are 'foo and 'bar the same type then?
For now, enums can only be text and they are assumed to be globals.

E.g., this is disallowed

```
GIVEN x IS ONE OF foo IS A NUMBER;
      bar IS A NUMBER
DECIDE f x IS 5
```

# Renaming

How do we want to rename a program?
Example programs:

**Id Function**

```
GIVEN x
DECIDE id x IS x
```

```
GIVEN x1
DECIDE id1 x1 IS x1
```

**Multiple Id Functions**

```
§ fun_0
GIVEN x
DECIDE id x IS x

§ fun_1
GIVEN x
DECIDE id2 x IS x
```

```
GIVEN x1
DECIDE id1 x1 IS x1

GIVEN x2
DECIDE id12 x2 IS x2
```

Note, `id1` is renamed to `id12`.
`GIVEN`s are renamed as they are local to the function.

Scope Tree

```
* (1) id
    * (2) x
* (3) id
    * (4) x
```

```
* 1 (id)
    * x
* 2 (id)
    *
