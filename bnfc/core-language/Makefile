all : Lexer.hs Parser.hs haskell

Lexer.hs: src/Lexer.x
	alex src/Lexer.x

Parser.hs: src/Parser.y
	happy src/Parser.y

#all : subdirs src-bnfc/AbsL.hs

subdirs :
	mkdir -p src-bnfc l4 out

src-bnfc/AbsL.hs : l4.bnfc
	(cd src-bnfc; bnfc -m ../l4.bnfc > ../out/bnfc.out 2> ../out/bnfc.err || stack run showbug ../l4.bnfc ../out/bnfc.out ../out/bnfc.err ; rm TestL.hs; make ParL.hs; rm ParL.hs)
	stack build
