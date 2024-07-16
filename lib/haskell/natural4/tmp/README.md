To run:

```
(base) ┌─[20240604-16:30:36]   [mengwong@rosegold:~/src/smucclaw/dsl/lib/haskell/natural4]
└─[0] <git:(main a0ecd7ff) > natural4-exe --workdir workdir tmp/LegalSS\ v0.9.7.0\ development\ -\ Must\ Sing\ 5.csv
natural4: will output asASP
0: firstPass = fromList []

gml2ml: not supported MkExp {exp = EIs {isLeft = MkExp {exp = EVar {var = MkVar "Drinks"}, md = [MkExpMetadata {srcPos = MkPosition {row = 3, col = 15}, typeLabel = Nothing, explnAnnot = Nothing}]}, isRight = MkExp {exp = EAnd {left = MkExp {exp = EOr {left = MkExp {exp = EVar {var = MkVar "an alcoholic"}, md = [MkExpMetadata {srcPos = MkPosition {row = 3, col = 15}, typeLabel = Nothing, explnAnnot = Nothing}]}, right = MkExp {exp = EOr {left = MkExp {exp = EVar {var = MkVar "a non-alcoholic"}, md = [MkExpMetadata {srcPos = MkPosition {row = 3, col = 15}, typeLabel = Nothing, explnAnnot = Nothing}]}, right = MkExp {exp = EEmpty, md = []}}, md = []}}, md = [MkExpMetadata {srcPos = MkPosition {row = 3, col = 15}, typeLabel = Just (Inferred "Boolean"), explnAnnot = Nothing}]}, right = MkExp {exp = EAnd {left = MkExp {exp = EOr {left = MkExp {exp = EVar {var = MkVar "in part"}, md = [MkExpMetadata {srcPos = MkPosition {row = 3, col = 15}, typeLabel = Nothing, explnAnnot = Nothing}]}, right = MkExp {exp = EOr {left = MkExp {exp = EVar {var = MkVar "in whole"}, md = [MkExpMetadata {srcPos = MkPosition {row = 3, col = 15}, typeLabel = Nothing, explnAnnot = Nothing}]}, right = MkExp {exp = EEmpty, md = []}}, md = []}}, md = [MkExpMetadata {srcPos = MkPosition {row = 3, col = 15}, typeLabel = Just (Inferred "Boolean"), explnAnnot = Nothing}]}, right = MkExp {exp = EEmpty, md = []}}, md = []}}, md = []}}, md = []}


gml2ml: not supported MkExp {exp = EIs {isLeft = MkExp {exp = EVar {var = MkVar "Qualifies"}, md = [MkExpMetadata {srcPos = MkPosition {row = 6, col = 7}, typeLabel = Nothing, explnAnnot = Nothing}]}, isRight = MkExp {exp = EAnd {left = MkExp {exp = EVar {var = MkVar "walks"}, md = [MkExpMetadata {srcPos = MkPosition {row = 6, col = 7}, typeLabel = Nothing, explnAnnot = Nothing}]}, right = MkExp {exp = EAnd {left = MkExp {exp = EOr {left = MkExp {exp = EVar {var = MkVar "Drinks"}, md = [MkExpMetadata {srcPos = MkPosition {row = 6, col = 7}, typeLabel = Nothing, explnAnnot = Nothing}]}, right = MkExp {exp = EOr {left = MkExp {exp = EVar {var = MkVar "eats"}, md = [MkExpMetadata {srcPos = MkPosition {row = 6, col = 7}, typeLabel = Nothing, explnAnnot = Nothing}]}, right = MkExp {exp = EEmpty, md = []}}, md = []}}, md = [MkExpMetadata {srcPos = MkPosition {row = 6, col = 7}, typeLabel = Just (Inferred "Boolean"), explnAnnot = Nothing}]}, right = MkExp {exp = EEmpty, md = []}}, md = []}}, md = []}}, md = []}


toMathLang: no giveth, returning all in symTab

natural4: output to workdir done

```

Now look inside output directory workdir/no-uuid/mathlang*


