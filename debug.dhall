let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "examples/debug/**/*.purs" ],
  dependencies = conf.dependencies # [ "partial", "console" ]
}
