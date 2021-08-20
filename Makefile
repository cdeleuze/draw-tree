
# this rule is platform independent
gtree.ml: tree_io.ml
	camlp4o -o $@ $^

tree: tree.cmo gtree.cmo text.cmo pictures.cmo main.cmo
	ocamlc -g graphics.cma $^ -o $@

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

OCAMLOPT=ocamlopt
WINOCAMLOPT=/usr/bin/i686-w64-mingw32-ocamlopt

tree.exe: tree.cmx gtree.cmx text.cmx pictures.cmx main.cmx
	make clean
	make OPT=win tree.opt
	mv tree.opt tree.exe

tree.opt: tree.cmx gtree.cmx text.cmx pictures.cmx main.cmx
ifeq ($(OPT),win)
	$(WINOCAMLOPT) graphics.cmxa $^ -o $@
else
	$(OCAMLOPT) -g graphics.cmxa $^ -o $@
endif

%.cmx : %.ml
ifeq ($(OPT),win)
	$(WINOCAMLOPT) -c $<
else
	$(OCAMLOPT) -c $<
endif

pdf: figures
	ocamlweb --header -s --noindex --class-options a4paper --latex-option novisiblespaces tree.ml > print.tex
	pdflatex print
	pdflatex print
	mv print.pdf tree.pdf
	rm print.*


# generate pictures for documentation
# we generate eps file, pdflatex will generate pdfs when needed

TREE=./tree

FIGS=centerp centern centers leftp rightp leftn rightn compact compact2 centerB align0 align1 align2 align3 manual0 manual6

EPSFIGS=$(foreach f,$(FIGS),$f.eps)

figures: $(EPSFIGS)

centerp.eps: fig.tree tree
	$(TREE) -oe $@ $<

centern.eps: fig.tree tree
	$(TREE) -oe $@ -e $<

centers.eps: fig.tree tree
	$(TREE) -oe $@ -s $<

leftp.eps: fig.tree tree
	$(TREE) -oe $@ -l $<

rightp.eps: fig.tree tree
	$(TREE) -oe $@ -r $<

leftn.eps: fig.tree tree
	$(TREE) -oe $@ -l -e $<

rightn.eps: fig.tree tree
	$(TREE) -oe $@ -r -e $<

compact.eps: fig.tree tree
	$(TREE) -oe $@ -c $<

compact2.eps: fact.tree tree
	$(TREE) -oe $@ -c $<

centerB.eps: centerB.fig
	fig2dev -Leps $< > $@

align0.eps: or2.tree tree
	$(TREE) -oe $@ $<

align1.eps: or2.tree tree
	$(TREE) -a 1 -oe $@ $<

align2.eps: or2.tree tree
	$(TREE) -a 2 -oe $@ $<

align3.eps: or2.tree tree
	$(TREE) -a 3 -oe $@ $<

manual0.eps: manual.tree tree
	$(TREE) -oe $@ -c $<

manual6.eps: manual.tree tree
	$(TREE) -oe $@ -d 6 $<

auto.eps: fact.tree tree
	$(TREE) -d 31 -d 38 -d 38 -d 39 -d 39 -d 41 -oe $@ $<


# other

fonts: fonts.ml
	ocamlc graphics.cma $< -o $@

test_text: test_text.ml
	ocamlc graphics.cma $< -o $@

fonts.exe: fonts.ml
	$(WINOCAMLOPT) graphics.cmxa $< -o $@

test_text.exe: test_text.ml
	$(WINOCAMLOPT) graphics.cmxa $< -o $@


tgz:
	tar cvzf ../tree.tgz

clean:
	rm -f *.cmo *.cmx *.cmi *.eps *-eps-converted-to.pdf tree tree.opt

