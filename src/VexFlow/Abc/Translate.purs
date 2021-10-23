module VexFlow.Abc.Translate
  ( keySignature
  , headerChange
  , notePitch
  , music
  ) where

-- | Translate between low-level leaves of the Abc data structure  and VexFlow types
-- | At the leaves, we don't need to thread context through the translation

import Data.Abc

import Data.Abc.Canonical (keySignatureAccidental)
import Data.Abc.KeySignature (normaliseModalKey)
import Data.Abc.Metadata (normaliseChord)
import Data.Array (concat, fromFoldable, last, length, mapWithIndex, (:))
import Data.Either (Either(..))
import Data.List (List, foldl)
import Data.List.NonEmpty (NonEmptyList, head, toUnfoldable) as Nel
import Data.Unfoldable (fromMaybe, replicate)
import Data.Maybe (Maybe(..), maybe)
import Data.Map (lookup)
import Data.String.Common (toLower)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Prelude ((<>), ($), (*), (+), (-), map, mempty, show)
import VexFlow.Abc.ContextChange (ContextChange(..), Clef(..))
import VexFlow.Abc.Slur (SlurBracket(..))
import VexFlow.Abc.TickableContext (NoteCount, TickableContext, getTickableContext)
import VexFlow.Abc.Utils
  ( normaliseBroken
  , noteDotCount
  , noteTicks
  , vexDuration
  , compoundVexDuration
  )
import VexFlow.Abc.Beat (exactBeatNumber)
import VexFlow.Abc.Repetition (buildRepetition)
import VexFlow.Types (AbcContext, BeatMarker, NoteSpec, TupletSpec, MusicSpec(..), VexDuration)

-- | generate a VexFlow indication of pitch
pitch :: PitchClass -> Accidental -> Int -> String
pitch pc acc oct =
  toLower (show pc) <> accidental acc <> "/" <> show oct

-- | return the VexFlow pitch of a note
notePitch :: AbcNote -> String
notePitch abcNote =
  -- VexFlow's notation of octave is one higher thab our ABC's
  pitch abcNote.pitchClass abcNote.accidental (abcNote.octave - 1)

-- set the arbitrary pitch of a rest, set to the middle of the appropriate stave
restPitch :: Clef -> String
restPitch = case _ of
  Bass ->
    pitch D Implicit 3
  Tenor ->
    pitch A Implicit 3
  Alto ->
    pitch C Implicit 4
  _ ->
    pitch B Implicit 4

accidental :: Accidental -> String
accidental Sharp = "#"
accidental Flat = "b"
accidental DoubleSharp = "##"
accidental DoubleFlat = "bb"
accidental Natural = "n"
accidental Implicit = ""

-- | return the VexFlow string representation of a note's accidental
noteAccidental :: AbcNote -> String
noteAccidental abcNote = accidental abcNote.accidental

-- | a key signature as a VexFlow String
-- | with 'strange' modes normalised to Major
keySignature :: KeySignature -> String
keySignature ks =
  let
    newks = case ks.mode of
      Major -> ks
      Minor -> ks
      _ -> normaliseModalKey ks
    modeStr = case newks.mode of
      Minor -> "m"
      _ -> ""
  in
    show newks.pitchClass <> (keySignatureAccidental newks.accidental) <> modeStr

-- | translate any ABC music item that produces score to a VexFlow music spec
-- | producing an empty array if it doesn't do so
-- | NoteCount is the count of 'tickable' items (notes or otherwise)
-- | that precede this music item in the bar
music :: AbcContext -> NoteCount -> Int -> NoteDuration -> Music -> Either String MusicSpec
music context tickablePosition noteIndex phraseDuration m =
  let
    -- find the number and size of 'tickable' items in this music item
    tickableContext :: TickableContext
    tickableContext = getTickableContext m
    -- find the fraction  of the bar that has already been processed
    barFraction = phraseDuration * context.unitNoteLength

    mBeatMarker :: Maybe BeatMarker
    mBeatMarker = exactBeatNumber barFraction context.beatDuration noteIndex
  in
    case m of
      Note gn ->
        buildMusicSpecFromN tickableContext noteIndex mBeatMarker gn.abcNote.tied gn.leftSlurs gn.rightSlurs
          $ graceableNote context gn

      Rest dur ->
        -- rests are never tied
        buildMusicSpecFromN tickableContext noteIndex mBeatMarker false 0 0 $ rest context dur

      Chord c ->
        -- we don't support tied chords at the moment
        buildMusicSpecFromN tickableContext noteIndex mBeatMarker false c.leftSlurs c.rightSlurs $ chord context c

      BrokenRhythmPair gn1 broken gn2 ->
        buildMusicSpecFromNs tickableContext noteIndex mBeatMarker gn1 gn2 $ brokenRhythm context gn1 broken gn2

      Tuplet t {- mGrace signature rOrNs -} ->
        -- grace notes prefacing tuplets currently ignored
        let
          eRes = tuplet context tickablePosition t.signature (Nel.toUnfoldable t.restsOrNotes)
        in
          -- we don't support tied tuplets at the moment
          map
            ( \tupletSpec ->
                MusicSpec
                  { noteSpecs: tupletSpec.noteSpecs
                  , tuplets: [ tupletSpec.vexTuplet ]
                  , ties:
                      -- if the note is tied then save the index into the note array
                      -- of the final note in the tuplet
                      if tupletSpec.tied then
                        [ noteIndex + length tupletSpec.noteSpecs - 1 ]
                      else
                        []
                  , tickableContext: tickableContext
                  , contextChanges: mempty
                  , slurBrackets: buildTupletSlurs noteIndex t.leftSlurs t.restsOrNotes
                  , beatMarkers: fromMaybe mBeatMarker
                  , repetitions: mempty
                  , typesettingSpaces: mempty
                  , chordSymbols: mempty
                  }
            )
            eRes

      ChordSymbol symbol ->
        -- we're not sure about chord symbols at the moment so it's a configurable option
        if (context.showChordSymbols) then
          Right $ buildMusicSpecFromChordSymbol symbol noteIndex
        else
          Right $ mempty :: Either String MusicSpec

      Inline header ->
        Right $ buildMusicSpecFromContextChange $ headerChange header

      DecoratedSpace decorations ->
        Right $ buildMusicSpecFromDecorations decorations noteIndex

      _ ->
        Right $ mempty :: Either String MusicSpec

-- | Translate an ABC graceable note to a VexFlow note
-- | failing if the duration cannot be translated
graceableNote :: AbcContext -> GraceableNote -> Either String NoteSpec
graceableNote context gn =
  let
    graceNotes :: Array AbcNote
    graceNotes = maybe [] (\grace -> Nel.toUnfoldable grace.notes) gn.maybeGrace

    graceKeys :: Array String
    graceKeys = map notePitch graceNotes

    graceAccidentals :: Array String
    graceAccidentals = map noteAccidental graceNotes
    -- edur = noteDur context gn.abcNote
    eVexDur = vexDuration context.unitNoteLength gn.abcNote.duration
    key = notePitch gn.abcNote
  in
    case eVexDur of
      Right vexDur ->
        let
          vexNote =
            { clef: show context.clef
            , keys: [ key ]
            , duration: compoundVexDuration vexDur
            , auto_stem: true
            }
        in
          Right
            { vexNote: vexNote
            , accidentals: [ accidental gn.abcNote.accidental ]
            , dots: [ vexDur.dots ]
            , graceKeys: graceKeys
            , graceAccidentals: graceAccidentals
            , ornaments: ornaments gn.decorations
            , articulations: articulations gn.decorations
            , noteTicks: noteTicks context.unitNoteLength gn.abcNote.duration
            , chordSymbol: ""
            }
      Left x -> Left x

-- | translate an ABC rest to a VexFlow note
-- | which we'll position on the B stave line 
-- | failing if the duration cannot be translated
rest :: AbcContext -> AbcRest -> Either String NoteSpec
rest context abcRest =
  let
    eVexDur = vexDuration context.unitNoteLength abcRest.duration
    -- key = pitch B Implicit 4
    key = restPitch context.clef
  in
    case eVexDur of
      Right vexDur ->
        let
          vexNote =
            { clef: show context.clef
            , keys: [ key ]
            , duration: (compoundVexDuration vexDur <> "r")
            , auto_stem: true
            }
        in
          Right
            { vexNote: vexNote
            , accidentals: []
            , dots: [ vexDur.dots ]
            , graceKeys: []
            , graceAccidentals: []
            , ornaments: []
            , articulations: []
            , noteTicks: noteTicks context.unitNoteLength abcRest.duration
            , chordSymbol: ""
            }
      Left x -> Left x

-- | translate an ABC chord to a VexFlow note
-- | failing if the durations cannot be translated
-- | n.b. in VexFlow, all notes in a chord must have the same duration
-- | this is a mismatch with ABC.  We just take the first note as representative
chord :: AbcContext -> AbcChord -> Either String NoteSpec
chord context abcChord0 =
  let
    abcChord = normaliseChord abcChord0

    -- this isn't quite right = we'll just used the length of the first
    -- note in the (normalised) chord
    chordLen :: NoteDuration
    chordLen = (Nel.head abcChord.notes).duration

    eVexDur :: Either String VexDuration
    -- eVexDur = chordalNoteDur context abcChord.duration (Nel.head abcChord.notes)
    eVexDur = vexDuration context.unitNoteLength chordLen

    keys :: Array String
    keys = map notePitch (Nel.toUnfoldable abcChord.notes)

    dotCounts :: Array Int
    dotCounts = map (noteDotCount context) (Nel.toUnfoldable abcChord.notes)

    accidentals :: Array String
    accidentals = map noteAccidental (Nel.toUnfoldable abcChord.notes)
  in
    case eVexDur of
      Right vexDur ->
        let
          vexNote =
            { clef: show context.clef
            , keys: keys
            , duration: compoundVexDuration vexDur
            , auto_stem: true
            }
        in
          Right
            { vexNote: vexNote
            , accidentals: accidentals
            , dots: dotCounts -- we need to apply dots to each note in the chord
            , graceKeys: []
            , graceAccidentals: []
            , ornaments: []
            , articulations: []
            , noteTicks: noteTicks context.unitNoteLength chordLen
            , chordSymbol: ""
            }
      Left x -> Left x

-- | translate an ABC broken note pair to a VexFlow note pair
-- | failing if either duration cannot be translated
-- | not finished - n1 can alter the context for n2
brokenRhythm :: AbcContext -> GraceableNote -> Broken -> GraceableNote -> Either String (Array NoteSpec)
brokenRhythm context gn1 broken gn2 =
  let
    (Tuple n1 n2) = normaliseBroken broken gn1 gn2
    enote1 = graceableNote context n1
    enote2 = graceableNote context n2
  in
    case (Tuple enote1 enote2) of
      (Tuple (Right note1) (Right note2)) ->
        Right [ note1, note2 ]
      (Tuple (Left e1) _) ->
        Left e1
      (Tuple _ (Left e2)) ->
        Left e2

-- | translate an ABC tuplet to a VexFlow tuplet spec
-- | failing if any note duration cannot be translated
tuplet :: AbcContext -> Int -> TupletSignature -> Array RestOrNote -> Either String TupletSpec
tuplet context startOffset signature rns =
  let
    enoteSpecs = sequence $ map (restOrNote context) rns
    isTied = case last rns of
      Just (Right gNote) ->
        gNote.abcNote.tied
      _ ->
        false
    vexTuplet =
      { p: signature.p
      , q: signature.q
      , startPos: startOffset
      , endPos: startOffset + (length rns)
      }
  in
    case enoteSpecs of
      Right noteSpecs ->
        Right
          { vexTuplet: vexTuplet
          , noteSpecs: noteSpecs
          , tied: isTied
          }
      Left x ->
        Left x

restOrNote :: AbcContext -> RestOrNote -> Either String NoteSpec
restOrNote context rOrn =
  case rOrn of
    Left abcRest ->
      rest context abcRest
    Right gn ->
      graceableNote context gn

-- | cater for an inline header (within a stave)
-- |  we need to cater for changes in key signature, meter, unit note length or voice
-- |  which all alter the translation context.  All other headers may be ignored
headerChange :: Header -> Array ContextChange
headerChange h =
  case h of
    Key mks ->
      [ KeyChange mks ]

    UnitNoteLength dur ->
      [ UnitNoteChange dur ]

    Meter maybeSignature ->
      case maybeSignature of
        Just meterSignature ->
          [ MeterChange meterSignature ]
        _ ->
          []

    Voice voiceDescription ->
      case lookup "clef" voiceDescription.properties of
        Just "Bass" -> [ ClefChange Bass ]
        Just "bass" -> [ ClefChange Bass ]
        Just "Tenor" -> [ ClefChange Tenor ]
        Just "tenor" -> [ ClefChange Tenor ]
        Just "Alto" -> [ ClefChange Alto ]
        Just "alto" -> [ ClefChange Alto ]
        {-
        Just "Treble" -> [ ClefChange Treble ]
        Just "treble" -> [ ClefChange Treble ]
        -}
        -- anything else at all defaults to treble
        _ -> [ ClefChange Treble ]
    _ ->
      []

-- | translate an ABC note decoration into a VexFlow note ornament
ornaments :: List String -> Array String
ornaments decorations =
  let
    f :: Array String -> String -> Array String
    f acc decoration =
      case decoration of
        "T" ->
          "tr" : acc
        "trill" ->
          "tr" : acc
        "turn" ->
          "turn" : acc
        "P" ->
          "upmordent" : acc
        "uppermordent" ->
          "upmordent" : acc
        "M" ->
          "mordent" : acc
        "lowermordent" ->
          "mordent" : acc
        _ ->
          acc
  in
    foldl f [] decorations

-- | translate an ABC note decoration into a VexFlow note articulation
articulations :: List String -> Array String
articulations artics =
  let
    f :: Array String -> String -> Array String
    f acc decoration =
      case decoration of
        "." -> -- staccato

          "a." : acc
        "upbow" -> -- up bow

          "a|" : acc
        "u" -> -- up bow

          "a|" : acc
        "downbow" -> -- down bow

          "am" : acc
        "v" -> -- down bow

          "am" : acc
        "L" -> -- accent

          "a>" : acc
        "accent" -> -- accent

          "a>" : acc
        "emphasis" -> -- accent

          "a>" : acc
        "H" -> -- fermata  (above)

          "a@a" : acc
        "fermata" -> -- fermata  (above)

          "a@a" : acc
        "tenuto" -> -- tenuto

          "a-" : acc
        _ ->
          acc
  in
    foldl f [] artics

buildMusicSpecFromNs
  :: TickableContext
  -> Int
  -> Maybe BeatMarker
  -> GraceableNote
  -> GraceableNote
  -> Either String (Array NoteSpec)
  -> Either String MusicSpec
buildMusicSpecFromNs tCtx noteIndex mBeatMarker gn1 gn2 ens =
  let
    slurBrackets =
      buildSlurBrackets noteIndex gn1.leftSlurs gn1.rightSlurs
        <> buildSlurBrackets (noteIndex + 1) gn2.leftSlurs gn2.rightSlurs
  in
    map
      ( \ns -> MusicSpec
          { noteSpecs: ns
          , tuplets: []
          , ties: []
          , tickableContext: tCtx
          , contextChanges: []
          , slurBrackets: slurBrackets
          , beatMarkers: fromMaybe mBeatMarker
          , repetitions: []
          , typesettingSpaces: []
          , chordSymbols: []
          }
      )
      ens

buildMusicSpecFromN
  :: TickableContext
  -> Int
  -> Maybe BeatMarker
  -> Boolean
  -> Int
  -> Int
  -> Either String NoteSpec
  -> Either String MusicSpec
buildMusicSpecFromN tCtx noteIndex mBeatMarker isTied slurStartCount slurEndCount ens =
  map
    ( \ns -> MusicSpec
        { noteSpecs: [ ns ]
        , tuplets: []
        , ties:
            -- if the note is tied then save its index into the note array
            if isTied then [ noteIndex ]
            else []
        , tickableContext: tCtx
        , contextChanges: []
        , slurBrackets: buildSlurBrackets noteIndex slurStartCount slurEndCount
        , beatMarkers: fromMaybe mBeatMarker
        , repetitions: []
        , typesettingSpaces: []
        , chordSymbols: []
        }
    )
    ens

buildMusicSpecFromContextChange :: Array ContextChange -> MusicSpec
buildMusicSpecFromContextChange contextChanges =
  let
    (MusicSpec contents) = mempty :: MusicSpec
  in
    MusicSpec contents { contextChanges = contextChanges }

buildMusicSpecFromDecorations :: List String -> Int -> MusicSpec
buildMusicSpecFromDecorations decorations noteIndex =
  let
    (MusicSpec contents) = mempty :: MusicSpec
    repetitions = map (buildRepetition noteIndex) (fromFoldable decorations)
  in
    MusicSpec contents { repetitions = repetitions, typesettingSpaces = [ noteIndex ] }

buildMusicSpecFromChordSymbol :: SymbolDefinition -> Int -> MusicSpec
buildMusicSpecFromChordSymbol symbol noteIndex =
  let
    (MusicSpec contents) = mempty :: MusicSpec
  in
    MusicSpec contents { chordSymbols = [ { name: symbol.name, noteIndex } ] }

-- | build the slur brackets from a normal note's left and right slur counts
buildSlurBrackets :: Int -> Int -> Int -> Array SlurBracket
buildSlurBrackets noteIndex startCount endCount =
  replicate startCount (LeftBracket noteIndex)
    <> (replicate endCount (RightBracket noteIndex))

-- | build any left-slur brackets that immediately preface the tuplet
buildTupletPrefaceSlurs :: Int -> Int -> Array SlurBracket
buildTupletPrefaceSlurs noteIndex startCount =
  buildSlurBrackets noteIndex startCount 0

-- | build a tuplet slur bracket from the notes inside the tuplet
buildInterTupletSlurs :: Int -> Nel.NonEmptyList RestOrNote -> Array SlurBracket
buildInterTupletSlurs noteIndex tupletNotes =
  let
    f :: Int -> RestOrNote -> Array SlurBracket
    f pos rOrN =
      case rOrN of
        Left _ -> [] --- rest
        Right gNote ->
          buildSlurBrackets (noteIndex + pos) gNote.leftSlurs gNote.rightSlurs
  in
    concat $ mapWithIndex f (Nel.toUnfoldable tupletNotes)

-- | build all the slurs associated with a tuplet
buildTupletSlurs :: Int -> Int -> Nel.NonEmptyList RestOrNote -> Array SlurBracket
buildTupletSlurs noteIndex prefaceSlurCount tupletNotes =
  buildTupletPrefaceSlurs noteIndex prefaceSlurCount <>
    (buildInterTupletSlurs noteIndex tupletNotes)