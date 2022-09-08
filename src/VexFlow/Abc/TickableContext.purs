module VexFlow.Abc.TickableContext where

-- | A 'tickable' item is a first class item that exists on a bar stave
-- | and occupies real estate - notably notes and rests.
-- | We need to take account of these in each bar ( for instance, to determine
-- | the start position amongst the tickables of any tuplet)

import Data.Abc
  ( Bar
  , Music(..)
  , Grace
  , KeySignature
  , NoteDuration
  , RestOrNote
  )
import Data.Abc.KeySignature (keySet)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldMap)
import Data.List (length)
import Data.List.NonEmpty (head, length, toUnfoldable) as Nel
import Data.Maybe (Maybe, maybe, fromMaybe)
import Data.Int (round, toNumber)
import Data.Rational ((%), fromInt)
import Prelude (class Monoid, class Semigroup, map, mempty, (*), (+), ($))

-- | The default horizontal separation between notes in a bar (measured
-- | in pixels at a standard scale of 1.0)
-- |
-- | The number of pixels we designate to a tickable item (aka note) on a stave.
-- | This affects the horizontal separation of notes and is a matter of taste.
-- | I like this look and feel with the value set to 30.0, but 35.0 is OK too 
-- | giving the notes a little more space. It (obviously) also affects the width of a bar.
-- |
-- | When we invoke VexFlow, we use the ```SOFT``` voice mode and we use the magic
-- | ```softmaxFactor: 5``` in the formatter.  This has the effect of letting us determine
-- | the bar width and the horizontal layout of notes within the bar and not VexFlow.
-- | However, this would perhaps not be appropriate if we had wanted to produce multi-staves
-- | with the correct vertical alignment of notes between the parts.  The alternative would 
-- | be to use the ```Full``` voice mode and to let VexFlow calculate the bar contents width
-- | and layout.
-- |
-- | This would then mean that VexFlow decides the horizontal note separation but the downside
-- | is that there can then be wasted space at the right-hand side of each bar because we use 
-- | our heuristic for the bar width when creating the actual bar stave.
defaultNoteSeparation :: Number
defaultNoteSeparation = 
  30.0

type NoteCount = Int
type GraceCount = Int

data TickableContext = TickableContext NoteCount GraceCount NoteDuration

instance tickableSemigroupCtx :: Semigroup TickableContext where
  append (TickableContext n1 g1 d1) (TickableContext n2 g2 d2) =
    TickableContext (n1 + n2) (g1 + g2) (d1 + d2)

instance tickableMonoidCtx :: Monoid TickableContext where
  mempty = TickableContext 0 0 (fromInt 0)

-- | get the tickable content of any Music item
-- | Grace notes don't count here for the VexFlow API
getTickableContext :: Music -> TickableContext
getTickableContext m =
  case m of
    Note gn ->
      TickableContext 1 (graceLength gn.maybeGrace) gn.abcNote.duration

    Rest { duration } ->
      TickableContext 1 0 duration

    Chord abcChord ->
      let
        abcNote = Nel.head abcChord.notes
        duration = abcChord.duration * abcNote.duration
      in
        TickableContext 1 0 duration

    BrokenRhythmPair gn1 _ gn2 ->
      TickableContext 2
        (graceLength gn1.maybeGrace + graceLength gn2.maybeGrace)
        (gn1.abcNote.duration + gn2.abcNote.duration)

    Tuplet t ->
      -- grace notes prefacing tuplets are currently ignored
      let
        reduction = t.signature.q % t.signature.p
        graceNoteLength = getRorNsGraceLength (Nel.toUnfoldable t.restsOrNotes)
        duration = reduction * (getRorNsDuration (Nel.toUnfoldable t.restsOrNotes))
      in
        TickableContext t.signature.r graceNoteLength duration

    _ ->
      mempty

-- we ignore the duration of the grace notes that preface a real note
-- because in effect their duration is 'borrowed' from the actual note
graceLength :: Maybe Grace -> Int
graceLength maybeGraceNote =
  fromMaybe 0 $ map (\g -> Nel.length g.notes) maybeGraceNote

getRorNsDuration :: Array RestOrNote -> NoteDuration
getRorNsDuration rOrNs =
  let
    f acc rOrN =
      case rOrN of
        Left rest -> rest.duration + acc
        Right graceableNote -> graceableNote.abcNote.duration + acc
  in
    foldl f (fromInt 0) rOrNs

getRorNsGraceLength :: Array RestOrNote -> GraceCount
getRorNsGraceLength rOrNs =
  let
    f acc rOrN =
      case rOrN of
        Left _ -> 0 + acc -- rest
        Right graceableNote -> -- note

          graceLength graceableNote.maybeGrace + acc
  in
    foldl f 0 rOrNs

-- | heuristic to estimate the width of a bar
estimateBarWidth :: Boolean -> Boolean -> Maybe KeySignature -> Number -> Bar -> Int
estimateBarWidth hasClef hasTimeSig maybeKeySig pixelsPerItem abcBar =
  let
    (TickableContext noteCount graceCount _duration) = 

      foldMap getTickableContext abcBar.music
    clefCount =
      -- a clef is a little fatter than a normal tickable
      if hasClef then 1.3 else 0.0
    timeSigCount =
      if hasTimeSig then 1.0 else 0.0
    keySigCount =
      maybe 0.0 keySignatureWidth maybeKeySig
  in
    round $
      ( clefCount
          + timeSigCount
          + keySigCount
          + (tickableCountWidth noteCount)
          + (0.5 * toNumber graceCount)
      ) * pixelsPerItem

-- heuristic to decide how much width to dedicate to a key signature
-- by counting the number of sharps and flats
keySignatureWidth :: KeySignature -> Number
keySignatureWidth keySignature =
  case (length $ keySet keySignature) of
    0 ->
      0.0
    1 ->
      1.0
    2 ->
      1.0
    3 ->
      1.5
    _ -> 
      2.0

-- | heuristic to allocate width to 'tickables'
tickableCountWidth :: Int -> Number
tickableCountWidth n =
  case n of
    1 -> 1.5 -- just 1.0 is too small
    2 -> 2.5 -- just 2.0 is too small
    _ -> toNumber n
