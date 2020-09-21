
all : subdirs out/test.out
	stack exec showbug l4/test.l4 out/test.out out/test.err

subdirs :
	mkdir -p src l4 out

out/test.out : l4/test.l4 src/AbsL.hs
	stack exec l4 l4/test.l4 > out/test.out 2> out/test.err || stack exec showbug l4/test.l4 out/test.out out/test.err

src/AbsL.hs : l4.bnfc
	(cd src; bnfc -m ../l4.bnfc > ../out/bnfc.out 2> ../out/bnfc.err || stack exec showbug ../l4.bnfc ../out/bnfc.out ../out/bnfc.err ; rm TestL.hs; make ParL.hs; rm ParL.hs)
	stack build

l4/test.l4 : l4/test1.l4
	cp l4/test1.l4 l4/test.l4

l4/test1.l4 l4.bnfc : ../README.org
	emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "../README.org")'

.PHONY: clean all subdirs
clean :
	rm out/* src/* l4/* prolog/*
