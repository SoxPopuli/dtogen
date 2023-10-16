(lang dune 3.9)

(name dtogen)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name dtogen)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   ocaml 
   dune
   (yojson (= 2.1.0))
   (ppx_yojson_conv (= 0.16.0))
 )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
