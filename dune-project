(lang dune 3.0)

(name struct-diff)

(generate_opam_files true)

(license ISC)
(authors "Nathan Rebours <nathan.p.rebours@gmail.com>")
(maintainers "Nathan Rebours <nathan.p.rebours@gmail.com>")
(source
 (github NathanReb/ocaml-struct-diff))

(package
 (name struct-diff)
 (synopsis "Structural diff for arbitrary OCaml types")
 (depends
  (ocaml (>= 4.08))))
