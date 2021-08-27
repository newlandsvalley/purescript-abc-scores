{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "abc-scores"
, dependencies =
  [ "abc-parser"
  , "arrays"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "rationals"
  , "strings"
  , "stringutils"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "debug"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
