(lang dune 3.17)

(name liteeffects)

(generate_opam_files true)

(using menhir 2.1)

(source
 (github pashutk/liteeffects))

(authors "Pavel Glushkov <me@pashutk.com>")

(maintainers "Pavel Glushkov <me@pashutk.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name liteeffects)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune alcotest cmdliner menhir)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
