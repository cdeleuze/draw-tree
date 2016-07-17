

EPS=centerp.eps centern.eps centers.eps leftp.eps rightp.eps leftn.eps rightn.eps compact.eps compact2.eps centerB.eps align0.eps align1.eps align2.eps align3.eps manual0.eps manual6.eps

eps: $(EPS)

# this rule is platform independent
text.ml: text_s.ml text.cmi
	camlp4o -o $@ $<

tree: tree.cmo text.cmo pictures.cmo main.cmo
	ocamlc -g graphics.cma $^ -o $@

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -g -c $<

OCAMLOPT=ocamlopt
WINOCAMLOPT=/usr/bin/i686-w64-mingw32-ocamlopt

tree.exe: tree.cmx text.cmx pictures.cmx main.cmx
	make clean
	make OPT=win tree.opt
	mv tree.opt tree.exe

tree.opt: tree.cmx text.cmx pictures.cmx main.cmx
ifeq ($(OPT),win)
	$(WINOCAMLOPT) -g graphics.cmxa $^ -o $@
else
	$(OCAMLOPT) -g graphics.cmxa $^ -o $@
endif

%.cmx : %.ml
ifeq ($(OPT),win)
	$(WINOCAMLOPT) -c $<
else
	$(OCAMLOPT) -c $<
endif

ps: eps
	ocps -2 tree


TREE=./tree

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


fonts: fonts.ml
	ocamlc graphics.cma $< -o $@

test_text: test_text.ml
	ocamlc graphics.cma $< -o $@

fonts.exe: fonts.ml
	$(WINOCAMLOPT) graphics.cmxa $< -o $@

test_text.exe: test_text.ml
	$(WINOCAMLOPT) graphics.cmxa $< -o $@

clean:
	rm -f *.cmo *.cmx *.cmi *.eps tree tree.opt text.ml

