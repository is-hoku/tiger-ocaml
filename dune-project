(lang dune 3.4)

(name tiger)

(generate_opam_files true)

(source
 (github is-hoku/tiger-ocaml))

(authors "Hoku Ishibe")

(maintainers "Hoku Ishibe")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tiger)
 (synopsis "Tiger Compiler")
 (description "This is a tiger compiler which refers to the book, Modern Compiler Implementation in ML")
 (depends ocaml dune))

(package
 (name straight_line_program)
 (synopsis "straight-line program")
 (description "straight-line program")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
