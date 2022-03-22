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
  , "minibench"
  , "nonempty"
  , "partial"
  , "prelude"
  , "strings"
  , "tailrec"
  , "transformers"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "bench/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/purescript-contrib/purescript-string-parsers"
}
