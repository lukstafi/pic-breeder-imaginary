(lang dune 3.0)

(name pic_breeder)

(generate_opam_files true)

(source
 (github lukstafi/pic-breeder-imaginary))

(authors "Pic Breeder Authors")

(maintainers "Pic Breeder Maintainers")

(license MIT)

(package
 (name pic_breeder)
 (synopsis "Evolutionary art using complex number expressions")
 (description "A clone of the Pic Breeder project that evolves complex number expressions as pictures")
 (depends
  (ocaml (>= 4.14.0))
  dune
  bogue
  tsdl))
