(lang dune 2.0)
(generate_opam_files true)

(name "gemini_client")
(authors c-cube)
(license MIT)

(package
  (name gemini_client)
  (synopsis "gemini client")
  (depends uri ssl containers))
