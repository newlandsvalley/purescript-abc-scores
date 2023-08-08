{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "abc-scores"
, dependencies =
  [ "abc-parser"
  , "arrays"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "js-bigints"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "rationals"
  , "safe-coerce"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/newlandsvalley/purescript-abc-scores"
}
