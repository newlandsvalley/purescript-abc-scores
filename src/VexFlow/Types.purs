module VexFlow.Types where

import Control.Comonad.Store (StoreT(..))
import Data.Abc (NoteDuration)

-- | the configuration of the VexFlow Canvas
type Config =
    { canvasDivId :: String
    , canvasWidth :: Int
    , canvasHeight :: Int
    , scale :: Number
    }

-- | the configuration of a Stave
type StaveConfig =
    { x :: Int
    , y :: Int
    , width :: Int
    , barNo :: Int
    }

-- | the time signature
type TimeSignature =
  { numerator :: Int
  , denominator :: Int
  }

-- | The ABC Context
type AbcContext =
  { timeSignature :: TimeSignature
  , unitNoteLength :: NoteDuration
  , beatsPerBeam :: Int
  }

-- | a complete specification of Note(s) for VexFlow
type NoteSpec =
  { vexNote :: VexNote
  , accidentals :: Array String
  , dots :: Array Int
  }

-- | A raw note that VexFlow understands
type VexNote =
  { clef :: String
  , keys :: Array String
  , duration :: String
  }
