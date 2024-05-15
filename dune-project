(lang dune 3.14)
(name sqaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Alex Noviello"
         "Andrew Noviello"
         "Simon Ilincev"
         "Eashan Vagish")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name sqaml)
 (synopsis "SQAML")
 (description "A SQL-like Database implemented completely in OCaml")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
