{ name = "string-parsers"
, dependencies =
  [ "arrays"
  , "assert"
  , "bifunctors"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "nonempty"
  , "prelude"
  , "psci-support"
  , "strings"
  , "tailrec"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
