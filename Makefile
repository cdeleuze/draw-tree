

EPS=centerp.eps centern.eps centers.eps leftp.eps rightp.eps leftn.eps rightn.eps compact.eps compact2.eps centerB.eps align0.eps align1.eps align2.eps align3.eps manual0.eps manual6.eps

eps: $(EPS)

tree: tree.cmo text.cmo pictures.cmo main.cmo
	ocamlc -g graphics.cma $^ -o $@

text.cmo : text.ml
	ocamlc -pp camlp4o -I +camlp4 -c $<

%.cmo : %.ml
	ocamlc -g -c $<

%.cmx : %.ml
	ocamlopt -c $<

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

clean:
	rm -f *.cmo *.cmx *.cmi *.eps tree

