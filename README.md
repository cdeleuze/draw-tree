# draw-tree

A tool to draw trees from a simple indented text format (see .tree
files for examples).

Can draw on screen, produce eps file (easily convertible to pdf) or
fig file (from xfig fame, easily convertible to mostly anything).

The tree drawing algorithm is litterate code in tree.ml.

The rest of the code is:

 * tree_io.ml: loading/saving generic (unformatted) trees
   * each node has a label and a list of attributes
 * text.ml: loading a tree (uses tree_io)
 * pictures.ml: generic format for simple pictures, with export to
   eps, fig, and screen
 * main.ml: the main file

Developped on GNU/Linux (it used to also work on Windows but needs
some work).

## building

On debian 11, you need packages make, ocaml, camlp4,
libgraphics-ocaml-dev, libcamlp4-ocaml-dev.

To build the litterate code pdf (`make pdf`) you also need package
ocamlweb.

To convert fig files to PNG or whatever, package fig2dev.

----
Licence is GPL v3 or any later version
