(lang dune 3.0)

(using menhir 2.1)

(name dabor)

(generate_opam_files false)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name dabor)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune llvm llvm.analysis)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
