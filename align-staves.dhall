let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "examples/align-staves/**/*.purs" ]
}
