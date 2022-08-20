module VexFlow.Types where

import Data.Abc (BarLine, NoteDuration, KeySignature, TimeSignature)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Prelude (class Eq, class Monoid, class Semigroup, mempty, (<>))
import VexFlow.Abc.ContextChange (ContextChange, Clef)
import VexFlow.Abc.Repetition (Repetition)
import VexFlow.Abc.Slur (SlurBracket, VexCurve)
import VexFlow.Abc.TickableContext (TickableContext, defaultNoteSeparation)
import VexFlow.Abc.Volta (VexVolta)

type RenderingError = String

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
staveSeparation = 110

-- | the depth of a title (when present)
titleDepth :: Int
titleDepth = 48

-- | a score of a simple monophonic tune
type MonophonicScore = (NonEmptyArray StaveSpec)

-- | a score which can fail to be produced
type VexScore = Either String MonophonicScore

-- | the configuration of the VexFlow Backend (SVG is preferred, or Canvas)
type Config =
  { parentElementId :: String -- DivID (SVG) or CanvasID (Canvas)
  , width :: Int
  , height :: Int
  , scale :: Number
  , isSVG :: Boolean -- true (SVG) or false (Canvas)
  , titled :: Boolean -- true if we are displaying a tune title   
  , noteSeparation :: Number -- a number indicating how close together the notes are in a bar
  , showChordSymbols :: Boolean -- temporary configuration option till we're happy with it
  }

defaultConfig :: Config
defaultConfig =
  { parentElementId: "canvas"
  , width: 1600
  , height: 800
  , scale: 0.8
  , isSVG: true
  , titled: true
  , noteSeparation: defaultNoteSeparation
  , showChordSymbols: false
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
  { name :: String -- the chord symbol name itself
  , noteIndex :: Int -- the index of the note at which to attach the symbol
  }

-- | The ABC Context
type AbcContext =
  { timeSignature :: TimeSignature
  , keySignature :: KeySignature
  , mTempo :: Maybe Tempo
  , unitNoteLength :: NoteDuration
  , clef :: Clef
  , staveNo :: Maybe Int
  , accumulatedStaveWidth :: Int
  , isMidVolta :: Boolean -- we've started but not finished a volta
  , isNewTimeSignature :: Boolean -- we need to display a changed time signature
  , maxWidth :: Int
  , pendingRepeatBegin :: Boolean -- begin repeat to be prepended to next stave
  , beatDuration :: NoteDuration -- the duration of one beat under the time signature
  , noteSeparation :: Number -- a number indicating how close together the notes are in a bar
  , showChordSymbols :: Boolean -- temporary configuration option till we're happy with it
  }

-- | a NoteSpec is passed to the VexFlow JavaScript and so types have to be simple
type NoteSpec =
  { vexNote :: VexNote
  , accidentals :: Array String
  , dotCount :: Int
  , graceKeys :: Array String
  , graceAccidentals :: Array String
  , ornaments :: Array String
  , articulations :: Array String
  , noteTicks :: Int -- the measurable duration of the note in ticks
  , chordSymbol :: String -- a chord symbol attached to the note. Empty string if absent.
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
    , chordSymbols: mempty -- free-standing chord symbols.  Not yet attached to any note.
    }

data LineThickness
  = Single
  | Double
  | NoLine

derive instance eqLineThickness :: Eq LineThickness

data BarFill
  = Empty
  | Partial
  | Full
  | OverFull

derive instance eqBarFill :: Eq BarFill

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
  , fill :: BarFill -- what sort of complement of notes according to the time signature?
  , volta :: Maybe VexVolta
  , timeSignature :: TimeSignature
  , beamSpecs :: Array BeamSpec
  , curves :: Array VexCurve --  curves representing slurs
  , musicSpec :: MusicSpec
  }

type StaveSpec =
  { staveNo :: Int
  , staveWidth :: Int -- the cumulative width of the stave bars
  , clefString :: String
  , keySignature :: KeySignature
  , isNewTimeSignature :: Boolean -- do we need to display a time signature?
  , mTempo :: Maybe Tempo -- the tempo marker
  , barSpecs :: Array BarSpec
  }
