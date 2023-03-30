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

(package
 (name binary_search_tree)
 (synopsis "Binary Search Tree")
 (description "binary search tree")
 (depends ocaml dune))

(package
 (name lexical_analysis)
 (synopsis "Lexical Analysis")
 (description "lexical analyzer")
 (depends ocaml dune))

(package
 (name syntactic_analysis)
 (synopsis "Syntactic Analysis")
 (description "syntactic analyzer")
 (depends ocaml dune))

(package
 (name semantic_action)
 (synopsis "Semantic Action")
 (description "syntactic analyzer with semantic action")
 (depends ocaml dune))

(package
 (name semantic_analysis)
 (synopsis "Semantic Analysis")
 (description "semantic analyzer")
 (depends ocaml dune))

(package
 (name activation_record)
 (synopsis "Activation Record")
 (description "activation record")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
