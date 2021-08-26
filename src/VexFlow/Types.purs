module VexFlow.Types where

import Data.Abc (BarLine, NoteDuration, KeySignature)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Prelude (class Eq, class Monoid, class Semigroup, mempty, (<>))
import VexFlow.Abc.ContextChange (ContextChange)
import VexFlow.Abc.Repetition (Repetition)
import VexFlow.Abc.Slur (SlurBracket, VexCurve)
import VexFlow.Abc.TickableContext (TickableContext)
import VexFlow.Abc.Volta (VexVolta)

type BeatNumber = Int

-- | the indentation of the stave from the left margin
staveIndentation :: Int
staveIndentation = 10

-- | the margin below the score
-- | (only used at the moment in alignment)
scoreMarginBottom :: Int
scoreMarginBottom = 15

-- | the distance between successive staves
staveSeparation :: Int
staveSeparation = 100

-- | the depth of a title (when present)
titleDepth :: Int
titleDepth = 48

type VexScore = Either String (Array (Maybe StaveSpec))

-- | the configuration of the VexFlow Backend (SVG is preferred, or Canvas)
type Config =
  { parentElementId :: String -- DivID (SVG) or CanvasID (Canvas)
  , width :: Int
  , height :: Int
  , scale :: Number
  , isSVG :: Boolean -- true (SVG) or false (Canvas)
  , titled :: Boolean -- true if we are displaying a tune title   
  }

-- | the configuration of a Stave
type StaveConfig =
  { x :: Int
  , y :: Int
  , width :: Int
  , barNo :: Int
  , lineColour :: String
  , hasRightBar :: Boolean
  , hasDoubleRightBar :: Boolean
  }

-- | the time signature
type TimeSignature =
  { numerator :: Int
  , denominator :: Int
  }

type VexDuration =
  { vexDurString :: String -- w,h,q,8,16 or 32
  , dots :: Int -- number of dots
  }

-- | the tempo marking
type Tempo =
  { duration :: String
  , dots :: Int
  , bpm :: Int
  }

-- a note that starts exactly on a beat
type BeatMarker =
  { beatNumber :: BeatNumber
  , noteIndex :: Int
  }

-- a chord symbol
type ChordSymbol = 
  { symbol :: String 
  , x :: Int
  }

-- | The ABC Context
type AbcContext =
  { timeSignature :: TimeSignature
  , keySignature :: KeySignature
  , mTempo :: Maybe Tempo
  , unitNoteLength :: NoteDuration
  , staveNo :: Maybe Int
  , accumulatedStaveWidth :: Int
  , isMidVolta :: Boolean -- we've started but not finished a volta
  , isNewTimeSignature :: Boolean -- we need to display a changed time signature
  , maxWidth :: Int
  , pendingRepeatBegin :: Boolean -- begin repeat to be prepended to next stave
  , beatDuration :: NoteDuration -- the duratio of one beat under the time signature
  }

type NoteSpec =
  { vexNote :: VexNote
  , accidentals :: Array String
  , dots :: Array Int
  , graceKeys :: Array String
  , graceAccidentals :: Array String
  , ornaments :: Array String
  , articulations :: Array String
  , noteTicks :: Int -- the measurable duration of the note in ticks
  }

-- | A raw note that VexFlow understands
type VexNote =
  { clef :: String
  , keys :: Array String
  , duration :: String
  , auto_stem :: Boolean
  }

-- | the specification of the layout of an individual tuplet in the stave
type VexTuplet =
  { p :: Int -- fit p notes
  , q :: Int -- into time allotted to q
  , startPos :: Int -- from the array of notes at this position..
  , endPos :: Int -- to this position
  }

-- | the specification of an individual tuplet
type TupletSpec =
  { vexTuplet :: VexTuplet
  , noteSpecs :: Array NoteSpec
  , tied :: Boolean -- is the last note in the tuplet tied
  }

type BeamSpec = Array Int -- must be of length 2 - from Index and to Index

-- | the specification of a music item or a bar of same
-- | we may just have note specs in either or we may have
-- | one tuple spec (in the case of a single tupinstance
-- | or many (in the case of a full bar of music items)
newtype MusicSpec = MusicSpec MusicSpecContents

instance musicSpecSemigroup :: Semigroup MusicSpec where
  append (MusicSpec ms1) (MusicSpec ms2) =
    MusicSpec (ms1 <> ms2)

instance musicSpecMonoid :: Monoid MusicSpec where
  mempty = MusicSpec
    { noteSpecs: mempty
    , tuplets: mempty
    , ties: mempty
    , tickableContext: mempty
    , contextChanges: mempty
    , slurBrackets: mempty
    , beatMarkers: mempty
    , repetitions: mempty
    , typesettingSpaces: mempty
    , chordSymbols: mempty
    }

data LineThickness
  = Single
  | Double
  | NoLine

derive instance eqLineThickness :: Eq LineThickness

-- | we define MusicSpecContents separately from MusicSpec
-- | because we need to pass it to JavaScript
type MusicSpecContents =
  { noteSpecs :: Array NoteSpec
  , tuplets :: Array VexTuplet
  , ties :: Array Int
  , tickableContext :: TickableContext
  , contextChanges :: Array ContextChange
  , slurBrackets :: Array SlurBracket -- brackets (L and R) demarking slurs
  , beatMarkers :: Array BeatMarker -- note indices marking exact beats
  , repetitions :: Array Repetition -- codas, seqnos etc from decorated spaces
  , typesettingSpaces :: Array Int -- note index for note following any 'y' typesetting space
  , chordSymbols :: Array ChordSymbol
  }

type BarSpec =
  { barNumber :: Int
  , width :: Int
  , xOffset :: Int
  , startLine :: BarLine -- the Left bar line (always present)
  , endLineThickness :: LineThickness -- right bar line type (default Single)?
  , endLineRepeat :: Boolean -- does it have an end repeat? important for end repeat markers
  , volta :: Maybe VexVolta
  , timeSignature :: TimeSignature
  , beamSpecs :: Array BeamSpec
  , curves :: Array VexCurve --  curves representing slurs
  , musicSpec :: MusicSpec
  }

type StaveSpec =
  { staveNo :: Int
  , staveWidth :: Int -- the cumulative width of the stave bars
  , keySignature :: KeySignature
  , isNewTimeSignature :: Boolean -- do we need to display a time signature?
  , mTempo :: Maybe Tempo -- the tempo marker
  , barSpecs :: Array BarSpec
  }
