
all : subdirs out/test.out pngs src-l4/Top.pgf
	stack run showbug l4/test.l4 out/test.out out/test.err

test : l4/test.l4 src-bnfc/AbsL.hs
	stack test

subdirs :
	mkdir -p src-bnfc l4 out

out/test.out : l4/test.l4 src-bnfc/AbsL.hs
	stack run -- l4 all < l4/test.l4 > out/test.out 2> out/test.err || stack run showbug l4/test.l4 out/test.out out/test.err
	dot -Tpng graph.dot > graph.png

src-bnfc/AbsL.hs : l4.bnfc src-l4/Top.hs
	(mkdir -p src-bnfc out; cd src-bnfc; export LC_ALL=C.UTF-8; bnfc -m ../l4.bnfc ; rm TestL.hs; make ParL.hs; rm ParL.hs)
	# (cd src-bnfc; bnfc --haskell ../l4.bnfc; rm TestL.hs)
	stack build

src-l4/Top.pgf src-l4/Top.hs : ../grammars/old/RuleEng.gf
	gf -make -f haskell -D src-l4 ../grammars/old/TopEng.gf

l4/test.l4 : l4/test1.l4
	cp l4/test1.l4 l4/test.l4

l4/test1.l4 l4.bnfc : ../README.org
	emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "../README.org")'

%.png : %.dot
	dot -Tpng $< > $@

pngs: ../graphviz/cabbage.png ../graphviz/pawnStates.png ../graphviz/knightStates.png

.PHONY: clean all test subdirs
clean :
	rm out/* src-bnfc/* l4/* prolog/*
