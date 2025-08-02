(lang dune 3.12)

(using melange 0.1)

(name relude)

(license "MIT")

(maintainers "Michael Martin" "Andy White" "John Haley")

(authors "Michael Martin" "Andy White" "John Haley")

(homepage https://github.com/reazen/relude)

(bug_reports https://github.com/reazen/relude/issues)

(package
 (name relude)
 (synopsis "Alternative standard library (prelude) for ReasonML")
 (description "Relude is a ReasonML/OCaml standard library replacement (prelude) written in ReasonML, targeting compilation to JavaScript via the Melange compiler. It provides typeclass-style abstractions inspired by Haskell, PureScript, Elm, and Scala cats/scalaz ecosystems.")
 (depends
  (ocaml (>= 5.1.0))
  (dune (>= 3.12))
  (melange (>= 5.1.0))
  "mel-bastet"
  (melange-jest :dev)
  (bisect_ppx :dev)
  (odoc (and (>= 1.5.0) :with-doc)))
 (tags ("ReasonML" "Melange" "functional programming" "prelude" "standard library")))
