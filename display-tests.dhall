let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "examples/display-tests/**/*.purs" ]
}
