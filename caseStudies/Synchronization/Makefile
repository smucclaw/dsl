all : untimed-petri.png multiPetri animpngs

untimed-petri.dot : README.org
	emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "README.org")'

# produces anim?-*.dot
multiPetri : mkMultiPetri.hs untimed-petri.dot
	stack mkMultiPetri.hs < untimed-petri.dot

# foreach *.dot, create a .png

animdots = $(basename $(wildcard anim*.dot))
animpngs =            $(addsuffix .png,  $(animdots))

animpngs : $(animpngs)
	convert -delay 100 anim1-?.png -loop 0 anim1.gif
	convert -delay 100 anim2-?.png -loop 0 anim2.gif
	convert -delay 100 anim3-?.png -loop 0 anim3.gif

%.png : %.dot
	dot -T png $< -o $@.tmp && mv $@.tmp $@


