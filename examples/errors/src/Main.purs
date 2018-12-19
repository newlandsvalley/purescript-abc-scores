module Examples.Errors.Main where

import Prelude (Unit, bind, ($), (/))
import Effect (Effect)
import Data.Tuple (Tuple(..))
import Data.Rational ((%))
import Data.Maybe (Maybe(..))
import Data.List (List(..), fromFoldable)
import Data.Array (toUnfoldable)
import Data.Int (round, toNumber)
import VexFlow.Score (initialiseCanvas, renderFullStave)
import VexFlow.Abc.Utils (cMajor)
import VexFlow.Types (Config, AbcContext, staveIndentation)
import Data.Abc (BodyPart(..), KeySignature, MeterSignature,
                 Thickness(..))
import Examples.Errors.Samples

canvasWidth :: Int
canvasWidth = 1200

scale :: Number
scale = 0.8

config :: Config
config =
  { canvasDivId : "canvas"
  , canvasWidth : canvasWidth
  , canvasHeight : 1600
  , scale : scale
  }

abcContext :: MeterSignature -> KeySignature -> Int -> AbcContext
abcContext (Tuple x y) keySignature staveNo =
  { timeSignature : { numerator: x, denominator: y }
  , keySignature : keySignature
  , unitNoteLength : ( 1 % 16)
  , staveNo : Just staveNo
  , accumulatedStaveWidth : staveIndentation
  , isMidVolta : false
  , isNewTimeSignature : false
  , maxWidth : round $ (toNumber canvasWidth / scale)
  , pendingGraceKeys : []
  , pendingRepeatBegin: false
  }

-- | we give each test it's own stave.  The downside is that subsequent staves
-- | do not by default display the time signature (because it's assumed to be inherited)

-- | bad beaming 4/4
-- | the last two notes should be beamed together
-- | this is because we use a beam group of 2/4 (1/2) biased towards standard
-- | reels and hornpipes
exampleNothing :: Effect Unit
exampleNothing =
  let
    staveNo = 0
    context0 = abcContext (Tuple 4 4) cMajor staveNo
    context = context0 { staveNo = Nothing }
    barType =
      { thickness : Thin
      , repeat : Nothing
      , iteration : Nothing
      }
    bar =
      { startLine : barType
      , music :  fromFoldable [c 2, f 2, g 2, g 2, f 4, enat 2, enat 2 ]
      }
    bodyPart = Score $ toUnfoldable [bar]
  in
    renderFullStave context bodyPart


main :: Effect Unit
main = do
  _ <- initialiseCanvas config
  exampleNothing

{-}
main :: Effect Unit
main = do
  _ <- initialise config
  example13
-}
