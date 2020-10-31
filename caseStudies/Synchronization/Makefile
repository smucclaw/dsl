gall : untimed-petri.png

untimed-petri.dot : README.org
	emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "README.org")'

%.png : %.dot
	dot -T png $< -o $@.tmp && mv $@.tmp $@


