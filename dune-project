(lang dune 3.3)

(name dl)

(generate_opam_files true)

(source (github lmdexpr/dl-from-zero))

(authors "Yuki Tajiri")

(maintainers "Yuki Tajiri <lmdexpr@gmail.com>")

(package
 (name dl)
 (synopsis "For study")
 (depends
   (ocaml (>= 4.14.0))
   ocamlformat
   ocaml-lsp-server
   dune
   core
   merlin
   jupyter))
