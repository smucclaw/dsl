all: LexL.hs ParL.hs test1

LexL.x : l4.bnfc
	bnfc $<

ParL.y : l4.bnfc
	bnfc $<

%.hs : %.x
	alex $<

%.hs : %.y
	happy $<

test1 : TestL.hs test1.l4 LexL.hs ParL.hs
	runghc TestL.hs < test1.l4
