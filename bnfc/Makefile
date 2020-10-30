
all : subdirs out/test.out pngs
	stack run showbug l4/test.l4 out/test.out out/test.err

subdirs :
	mkdir -p src-bnfc l4 out

out/test.out : l4/test.l4 src-bnfc/AbsL.hs
	stack exec l4 l4/test.l4 > out/test.out 2> out/test.err || stack run showbug l4/test.l4 out/test.out out/test.err
	dot -Tpng graph.dot > graph.png

src-bnfc/AbsL.hs : l4.bnfc
	(cd src-bnfc; bnfc -m ../l4.bnfc > ../out/bnfc.out 2> ../out/bnfc.err || stack run showbug ../l4.bnfc ../out/bnfc.out ../out/bnfc.err ; rm TestL.hs; make ParL.hs; rm ParL.hs)
	stack build

l4/test.l4 : l4/test1.l4
	cp l4/test1.l4 l4/test.l4

l4/test1.l4 l4.bnfc : ../README.org
	emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "../README.org")'

%.png : %.dot
	dot -Tpng $< > $@

pngs: ../graphviz/cabbage.png ../graphviz/pawnStates.png ../graphviz/knightStates.png

.PHONY: clean all subdirs
clean :
	rm out/* src-bnfc/* l4/* prolog/*
