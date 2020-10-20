docx_ = $(basename $(wildcard ycdc/*.docx))
org_  = $(addsuffix .org,$(docx_))
txt_  = $(addsuffix .txt,$(docx_))
md_   = $(addsuffix .md,$(docx_))

all : ycdc $(org_) $(txt_) $(md_)

clean :
	rm -rf ycdc

ycdc :
	mkdir -p ycdc
	( cd ycdc; wget -m -nd -c https://www.ycombinator.com/documents -I /assets/ycdc/; rename 's/ /-/g' *.docx )
	@echo "Now, you probably want to run    make    again to generate the output formats"

# Postmoney Safe - Discount Only v1.1-3d18bf3e3967c8e09c1b5a0318e8629d91358eae6986bee6d8a3acf2ec18c6ce.docx
# Postmoney Safe - MFN Only v1.1-e34548a382b732f12461b19fea3da24f06873d1903b4b55bf13aa25bb55911f3.docx
# Postmoney Safe - Valuation Cap Only v1.1-5e6f7dd124b848071137eae5e4630b2edbe2c15e5d62583646526766793585ed.docx
# Postmoney Safe - Valuation Cap and Discount v1.1-76b7ee5654ac178bca20b21eab5d3eba0c1da7c467aee4835f769263f8a18749.docx

%.org : %.docx
	pandoc $< -o $@

%.md : %.docx
	pandoc $< -o $@

%.txt : %.docx
	pandoc $< -o $@
