{ name = "string-parsers"
, dependencies =
  [ "arrays"
  , "assert"
  , "bifunctors"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "lists"
  , "math"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "strings"
  , "tailrec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
