(lang dune 3.14)

(using melange 0.1)

(name melange_tea)

(generate_opam_files true)

(source
 (github PrattFall/melange-tea))

(authors "OvermindDL1" "PrattFall")

(maintainers "PrattFall")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (allow_empty)
 (name melange_tea)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
