module VexFlow.Abc.TickableContext where

-- | A 'tickable' item is a first class item that exists on a bar stave
-- | and occupies real estate - notably notes and rests.
-- | We need to take account of these in each bar ( for instance, to determine
-- | the start position amongst the tickables of any tuplet)

import Data.Abc (Bar, Music(..), KeySignature, NoteDuration, RestOrNote)
import Data.Abc.KeySignature (keySet)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldMap)
import Data.List (length)
import Data.List.NonEmpty (head, toUnfoldable) as Nel
import Data.Maybe (Maybe, maybe)
import Data.Int (round, toNumber)
import Data.Rational ((%), fromInt)
import Prelude (class Monoid, class Semigroup, mempty, (*), (+), ($))

-- | the number of pixels we designate to a tickable item on a stave
pixelsPerItem :: Number
pixelsPerItem = 35.0

type NoteCount = Int

data  TickableContext = TickableContext NoteCount NoteDuration

instance tickableSemigroupCtx :: Semigroup TickableContext where
  append (TickableContext c1 d1) (TickableContext c2 d2) =
    TickableContext (c1 + c2) (d1 + d2)

instance tickableMonoidCtx :: Monoid TickableContext where
  mempty = TickableContext 0 (fromInt 0)

-- | get the tickable content of any Music item
getTickableContext :: Music -> TickableContext
getTickableContext m =
  case m of
    Note abcNote ->
      TickableContext 1 abcNote.duration

    Rest { duration } ->
      TickableContext 1 duration

    Chord abcChord ->
      let
        abcNote = Nel.head abcChord.notes
        duration = abcChord.duration * abcNote.duration
      in
        TickableContext 1 duration

    BrokenRhythmPair abcNote1 broken abcNote2 ->
      TickableContext 2 (abcNote1.duration + abcNote2.duration)

    Tuplet signature rOrNs ->
      let
        reduction = signature.q % signature.p
        duration = reduction * (getRorNsDuration (Nel.toUnfoldable rOrNs))
      in
        TickableContext signature.r duration

    _ ->
      mempty


getRorNsDuration :: Array RestOrNote -> NoteDuration
getRorNsDuration rOrNs =
  let
    f acc rOrN =
      case rOrN of
        Left rest -> rest.duration + acc
        Right note -> note.duration + acc
  in
    foldl f (fromInt 0) rOrNs

-- | heuristic to estimate the width of a bar
estimateBarWidth :: Boolean -> Boolean -> Maybe KeySignature -> Bar -> Int
estimateBarWidth hasClef hasTimeSig maybeKeySig abcBar =
  let
    (TickableContext noteCount duration) = foldMap getTickableContext abcBar.music
    clefCount =
      if hasClef then 1.0 else 0.0
    timeSigCount =
        if hasTimeSig then 1.0 else 0.0
    keySigCount =
      maybe 0.0 keySignatureWidth maybeKeySig
  in
    round $ (clefCount + timeSigCount + keySigCount + (tickableCountWidth noteCount)) * pixelsPerItem

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
    _ ->
      1.5

-- | heuristic to allocate width to 'tickables'
tickableCountWidth :: Int -> Number
tickableCountWidth n =
  case n of
    1 -> 1.5   -- just 1.0 is too small
    2 -> 2.5   -- just 2.0 is too small
    _ -> toNumber n
