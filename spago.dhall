{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "abc-scores"
, dependencies = [ "abc-parser", "console", "effect", "prelude"  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
