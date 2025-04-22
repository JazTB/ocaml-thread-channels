(lang dune 3.17)

(name Tc)

(generate_opam_files true)

(source
 (github JazTB/ocaml-thread-channels))

(authors "JazTB@example.com")

(maintainers "JazTB@example.com")

(license MIT)

(documentation https://example.com)

(package
 (allow_empty)
 (name Tc)
 (synopsis "Useful threading modules.")
 (description "Implements an easy to use channel api, and a mutex type that encapsulates a reference to some value.")
 (depends ocaml)
 (tags
  ("threading" "channels")))
