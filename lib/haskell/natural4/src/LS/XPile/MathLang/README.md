
# Summary of key L4-MathLang constructs and their translations

We tried to spec out a grammar from the 4-6 scattered examples of the Math Lang L4 dialect that Meng had given me.

This is quite adhoc and informal, and more syntactic than semantic. It's mostly just us (YM and Inari) thinking through the surface syntax of Meng's examples, with an eye towards the target semantics (lambda calculus; Meng's Math Lang). In particular, YM was trying to collate and summarize a concise list of the key patterns and constructs; this will help us make sure the translations are consistent and feasible, in addition to serving as a useful cheatsheet when implementing the destructuring and parsing.

That said, hopefully it will be useful as a kind of spec for the MathLang dialect too.

## Grammar

```
TypeSig  -> GIVEN [(TypeDecl|VarName)]{1,} GIVETH (TypeDecl|VarName)

TypeDecl -> VarName IS A Type


Head  -> TypeSig Head'
Head' ->     HSet:  DECIDE <VarName> IS <MathArg> 
          |  H???:  DECIDE <VarName> IS <MathExpr> 
          -- ^ distinguishing this from above because we'll need to use a different pattern synonym for this
          |  HPred: DECIDE  <VarName> Function
                {-  hHead = RPMT
                        [ MTT "ind"
                        , MTT "qualifies a la case 4"] -}
          |  DECIDE <VarName> 

--Function: looks in spreadsheet like | <arg> |_newcell <fn name> | --  "meets the property eligibility criteria for GSTV-Cash", "qualifies a la case 4"
MathArg -> Lit | VarName 
MathExpr ->  MathBinOp MathArg MathArg 
           -- ^ RPConstraint (foo) RPRel (bar) and you need to parse the contents of the bar
           {- eg:
            { hHead = RPConstraint
                                    [ MTT "n3a" ] RPis
                                    [ MTT "n1"
                                    , MTT "+"
                                    , MTT "n2"
                                    ]}
                
            -}
         | MathBinOp (MathBinOp MathArg MathArg) (MathBinOp MathArg MathArg)
         | MathBinOp (MathBinOp MathArg MathArg) MathArg


Body -> [Cond]{1,}
Cond  -> BVarE:   IF VarName IS A VarName -- refinement: where the VarName on the right is going to be the name of a class 
        --^ IS A is parsed into RPParamText, not RPConstraint, so it looks like this
        {- hBody = Just ( All Nothing
                    [ Leaf
                        ( RPParamText
                            (
                                ( MTT "ind" :| []
                                , Just
                                    ( SimpleType TOne "Singapore citizen" )
                                ) :| [] 
        -}
        | IF Propn                         -- these are parsed as RPConstraints usually, but not always
        | IF VarName Function            
        | OTHERWISE  
        -- YM: i think the pattern for the if ... if ... otherwise might be smtg like: some clauses followed by an otherwise
        -- maybe use the `traverse toList of the getAndOrTrees` stuff?


Propn <- VarName's <fieldname> <Op> <arg> 
       | cells with strings
       | arg <Op> arg
        | <LogicalOp> <Propn>
{-  
--^ Example of arg <op> arg: RPConstraint with one of the arith comp RPRels, e.g. RPConstraint [ MTT "ind's", MTT "age"] RPgte [ MTI 21 ]

( RPConstraint
                [ MTT "ind's" -- this was in one cell
        , MTT "place of residence" -- this was in another cell
        ] RPis
        [ MTT "Singapore" ]
    )
    ( RPConstraint
        [ MTT "ind's"
        , MTT "age"
        ] RPgte [ MTI 21 ] )
 -}


HC -> Head Body{0,1} [Where]{0,1}  

```

Let's maybe reserve IS A for class?



### Ideas for how to translate (very draft; WIP)

Re functions:
* If (i) `v` appears as a GIVEN and (ii) we have `DECIDE | v | somestr` in the head, `somestr` is a function name

Quick thought about record access and mutation
* when translating to Meng's AST, don't bother trying to do proper type inference for this --- just do it at the level of the var name when translating to his input format, with `<recordName.accessor>`


`DECIDE <variable> IS <Lit | MathBinOp ... | VarName >` 
* In the VarName case, it'd be translated to: -||- `"<variable>" @|. <expr>`

`DECIDE <VarName> IF <Prop1> <Op> <Prop2> ...`
* I'll take this to be sugar for: `DECIDE <VarName> IS True IF <Prop1> <Op> <Prop2> ...`

#### thinking abt the if then else; eedge cases / examples to think abt

```
DECIDE 	laughs	IS 	2				
	laughs	IS	0	IF 	cond1				
	sadds	IS	bleh	OTHERWISE					
	laughs	IS	77						
									
	laughs	IS	0	IF 	cond1				
	sadds	IS	bleh	OTHERWISE					
````

presumably analogous to

```
let sadds;
let laughs := 2;
if cond1 { 
  laughs := 0 
} else {
  sadds := bleh

laughs := 77"	
```

One idea: Use a parser combinator library that can work with arbitrary input types (e.g. https://github.com/nasso/comparse#readme) to further parse the L4 AST

If doing it more manually, the putative algo:

Two key cases:
1. IS without IF	

2. IS with IF	

    2a. Is there an OTHERWISE somewhere further down the list; if so, group the clauses from this to that OTHERWISE clause and treat that as an IF THEN ELSE construct	

    2b. If no OTHERWISE: then treat it as If Then Else with a Null / Void / None Else	


# On the transpilation process

The pipeline I'm thinking of:

`L4 -> Generic Math Lang (generic lam calc) -> Meng Math Lang`

Generic Math Lang can also then be exported independently, for other MathLang-like evaluators

