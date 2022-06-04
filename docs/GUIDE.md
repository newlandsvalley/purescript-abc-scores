# ABC Scores Guide

```abc-scores``` is a library dedicated for use in the browser that will display a score from a tune in ABC format once it has been parsed by the ```abc-parser```. It does this by wrapping [VexFlow 4](https://github.com/0xfe/vexflow). With VexFlow, you can choose to render your score using either ```Canvas``` or ```SVG``` - the result looks identical whichever you choose.  In order to start, you must first provide a labelled ```Div``` HTML node.  VexFlow will then provide an image under this node by side-effect.

## Configuration

Here is the default configuration used by ```abc-scores```:

```purs
defaultConfig :: Config
defaultConfig =
  { parentElementId: "canvas"
  , width: 1600
  , height: 800
  , scale: 0.8
  , isSVG: true
  , titled: true
  , showChordSymbols: false
  }
```

As you can see, SVG is chosen by default and it renders to a ```Div``` node labelled ```canvas```.  There are default pixel sizes for both width and height and you can override all these defaults with your own values.

You must provide target ```HTML```with this node (which can of course be provided statically or with a UI such as ```Halogen```. Here is a basic ```index.html``` file.  Notice that it firstly loads the VexFlow 4 script and that the example PureScript code we will write is transpiled to ```example.js```.

```purs
<!DOCTYPE html>
<html>
  <head>
    <title>ABC Scores Example</title>
    <script src="vexflow.js"></script>
  </head>
  <body>
    <div id="canvas" ></div>
    <script type="text/javascript" src="example.js"></script>
  </body>
</html>
```

## Dependencies

```purs
dependencies =
  [ "abc-parser"
  , "abc-scores"
  , "console"
  , "effect"
  , "either"
  , "prelude"
  ]
```

## Display a Finished Score 

```purs
module Example.Main where

import Prelude (bind, pure)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (renderFinalTune, initialiseCanvas)
import VexFlow.Types (defaultConfig)
import Data.Abc.Parser (parse)

main :: Effect Boolean
main =
  case (parse tuneText) of
    Right abcTune -> do
      renderer <- initialiseCanvas defaultConfig
      renderFinalTune defaultConfig renderer abcTune
    _ ->
      pure false

-- | the tune in ABC notation
tuneText :: String 
tuneText = ....
```

```renderFinalTune``` ignores the width and height settings of the configuration and instead displays the tune with staves justified and the canvas clipped to the dimensions of the score so as not to lose any real estate.  If the tune has a title (which it should) then it is displayed above the score.

## Display an Unfinished score

You may use ```purescript-scores``` in an editor application where the score is redisplayed after each keystroke.  In this case, the staves extend fully to the right (as governed by the ```width``` setting) and the notes usually don't fill the stave (unless they overflow the width).

```purs
module Example.Main where

import Prelude (bind, pure)
import Effect (Effect)
import Data.Either (Either(..))
import VexFlow.Score (renderTune, initialiseCanvas)
import VexFlow.Types (Config, defaultConfig)
import Data.Abc.Parser (parse)
import Data.Abc (AbcTune)

config :: Config
config =
  defaultConfig 
    { width = 1200
    , showChordSymbols = true 
    }

main :: Effect Boolean
main =
  case (parse tuneText) of
    Right abcTune -> do
      renderer <- initialiseCanvas config
      renderTune config renderer abcTune
    _ ->
      pure false
      
-- | the tune in ABC notation
tuneText :: String 
tuneText = ....
``` 
In this example, we override the default configuration to define the stave width and to request that chord symbols are displayed.  ```renderTune``` does not attempt to clip the canvas to the score size or to align the rightmost bar lines in the staves.

## Display a Thumbnail

A ```thumbnail``` is a picture of the introductory bars of a tune:

```purs
module Example.Main where

import Data.Abc.Parser (parse)
import Data.Either (Either(..))
import Effect (Effect)
import Prelude (bind, pure)
import VexFlow.Score (renderThumbnail, initialiseCanvas)
import VexFlow.Types (Config, defaultConfig)

config :: Config
config = defaultConfig 
  { width = 500
  , height = 200
  , isSVG = false
  , titled = false 
  }

main :: Effect Boolean
main =
  case (parse tuneText) of
    Right abcTune -> do
      renderer <- initialiseCanvas config
      renderThumbnail config renderer abcTune 
    _ ->
      pure false

-- | the tune in ABC notation
tuneText :: String 
tuneText = ....
```