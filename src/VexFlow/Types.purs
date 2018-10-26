module VexFlow.Types where

import Prelude (class Semigroup, class Monoid, (<>), mempty)
import Data.Maybe (Maybe)
import Data.Abc (BarType, NoteDuration)
import VexFlow.Abc.TickableContext (TickableContext)
import VexFlow.Abc.ContextChange (ContextChange)

staveIndentation :: Int
staveIndentation = 10

-- | just temporary while we have fixed width bars
staveWidth :: Int
staveWidth = 250

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
  , staveNo :: Maybe Int
  , accumulatedStaveWidth :: Int
  }

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

-- | the specification of the layout of an individual tuplet in the stave
type VexTuplet =
  { p :: Int           -- fit p notes
  , q :: Int           -- into time allotted to q
  , startPos :: Int    -- from the array of notes at this position..
  , endPos :: Int      -- to this position
  }

-- | the specification of an individual tuplet
type TupletSpec =
  { vexTuplet :: VexTuplet
  , noteSpecs :: Array NoteSpec
  }

-- | the specification of a music item or a bar of same
-- | we may just have note specs in either or we may have
-- | one tuple spec (in the case of a single tupinstance
-- | or many (in the case of a full bar of music items)
newtype MusicSpec = MusicSpec MusicSpecContents

instance musicSpecSemigroup :: Semigroup MusicSpec  where
  append (MusicSpec ms1) (MusicSpec ms2) =
    MusicSpec (ms1 <> ms2)

instance musicSpecMonoid:: Monoid MusicSpec where
  mempty = MusicSpec
    { noteSpecs : []
    , tuplets : []
    , ties : []
    , tickableContext : mempty
    , contextChanges : mempty
    }


-- | we define MusicSpecComntents separately from MusicSpec
-- | because we need to pass it to JavaScript
type MusicSpecContents =
  { noteSpecs :: Array NoteSpec
  , tuplets :: Array VexTuplet
  , ties :: Array Int
  , tickableContext :: TickableContext
  , contextChanges :: Array ContextChange
  }

type BarSpec =
  { barNumber :: Int
  , width  :: Int
  , xOffset :: Int
  , startLine :: BarType
  , endLineRepeat :: Boolean   -- important for end repeat markers
  , timeSignature :: TimeSignature
  , beatsPerBeam :: Int
  , musicSpec :: MusicSpec
  }

type StaveSpec =
  { staveNo :: Int
  , barSpecs :: Array BarSpec
  }
