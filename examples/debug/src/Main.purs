module Main where

-- debug the definition of the first BarSpec generated by the tune

import Prelude (Unit, bind, map, pure, show, unit, ($), (<>))
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Either (Either(..), fromRight')
import Data.Maybe (Maybe(..))
import Data.Array (head)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head) as NEA
import Data.Foldable (intercalate)
import VexFlow.Abc.Utils (initialAbcContext)
import VexFlow.Abc.TickableContext (TickableContext(..))
import VexFlow.Abc.TranslateStateful (runTuneBody)
import VexFlow.Types (BarSpec, BeamSpec, BeatMarker, Config,
          MusicSpec(..), NoteSpec, StaveSpec, VexTuplet, Titling(..), defaultConfig)
import Data.Abc.Parser (parse)
import Partial.Unsafe (unsafeCrashWith)

canvasWidth :: Int
canvasWidth = 1200

canvasHeight :: Int
canvasHeight = 1600

config :: Config
config =
  defaultConfig 
    { width = canvasWidth
    , height = canvasHeight 
    , titling = NoTitle
    }

debugFirstBar :: String -> String
debugFirstBar text =
  case (parse text) of
    Right abcTune ->
      let
        abcContext = fromRight' (\_ -> unsafeCrashWith "Unexpected Left") $ initialAbcContext abcTune config
      in
        case (runTuneBody abcContext abcTune.body) of
          Right amss ->
            debugStaveSpecs amss
          Left err ->
            "Translation error: " <> err
    _ ->
        "ABC parse error"

debugStaveSpecs :: NonEmptyArray StaveSpec -> String
debugStaveSpecs ass =
  let 
    ss = NEA.head ass
  in
    debugStaveSpec ss

debugStaveSpec :: StaveSpec -> String
debugStaveSpec staveSpec =
  debugBarSpecs staveSpec.barSpecs

debugBarSpecs :: Array BarSpec -> String
debugBarSpecs bss =
  case (head bss) of
    Just bs -> debugBarSpec bs
    _ ->
      "empty bar specs in first stave"

debugBarSpec :: BarSpec -> String
debugBarSpec barSpec =
  debugMusicSpec barSpec.musicSpec
    <> debugBeams barSpec.beamSpecs


debugMusicSpec :: MusicSpec -> String
debugMusicSpec (MusicSpec ms) =
   "Notes!: " <> (intercalate ", " $ map debugNoteSpec ms.noteSpecs) <> "\r\n"
   <>
  "Tuplets: " <> (intercalate ", " $ map debugTuplet ms.tuplets) <> "\r\n"
   <>
  "Beat markers: " <> (intercalate ", " $ map debugBeatMarker ms.beatMarkers) <> "\r\n"
   <> 
   "tickable note duration: " <> debugNoteDuration ms.tickableContext <> "\r\n"

debugNoteSpec :: NoteSpec -> String
debugNoteSpec ns =
    intercalate "-" ns.vexNote.keys <> " " <> debugGrace ns

debugTuplet :: VexTuplet -> String
debugTuplet vt =
   "(" <> show vt.p <> ":" <> show vt.q <> " (notes slice: "
     <> show vt.startPos <> "-" <> show vt.endPos <> ")"

debugBeatMarker :: BeatMarker -> String
debugBeatMarker bm =
  show bm.beatNumber <> "-" <> show bm.noteIndex


debugBeams :: Array BeamSpec -> String
debugBeams beams =
  "Beams: " <> "[" <> (intercalate "," $ map debugBeam beams) <> "]"

debugBeam :: BeamSpec -> String
debugBeam beam =
  "[" <> (intercalate "," $ map show beam) <> "]"

debugGrace :: NoteSpec -> String
debugGrace ns =
    "grace: " <> (intercalate "-" ns.graceKeys) <> 
    " grace accidentals: {" <>  (intercalate "-" ns.graceAccidentals) <> "}"

debugNoteDuration :: TickableContext -> String
debugNoteDuration (TickableContext _ _ duration) = 
  show duration

sampleAbc1 :: String
sampleAbc1 =
    "X:1\r\n"
    <> "T: beaming 2/4 with triplet\r\n"
    <> "M: 2/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: Gm\r\n"
    <> "A2B2 (3c2d2e2 |\r\n"

sampleAbc2 :: String
sampleAbc2 =
    "X:1\r\n"
    <> "T: simple 4/4\r\n"
    <> "M: 4/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: G\r\n"
    <> "A2B2 c2d2 A2B2 c2d2 |\r\n"

sampleAbc3 :: String
sampleAbc3 =
    "X:1\r\n"
    <> "T: lead in 16th\r\n"
    <> "M: 2/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: Gm\r\n"
    <> "ABc |\r\n"

sampleAbc4 :: String
sampleAbc4 =
    "X:1\r\n"
    <> "T: minim layout\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: C\r\n"
    <> "| c4 de |\r\n"     
    
sampleAbc5 :: String
sampleAbc5 =
    "X:1\r\n"
    <> "T: slip jig\r\n"
    <> "M: 9/8\r\n"
    <> "L: 1/16\r\n"
    <> "K: C\r\n"
    <> "A2B2c2 B2c2d2 c2d2e2 |\r\n"     

sampleAbc6 :: String
sampleAbc6 =
    "X:1\r\n"
    <> "T: spacing\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: C\r\n"
    <> "| C6 |\r\n"     

sampleAbc :: String
sampleAbc =
    "X:1\r\n"
    <> "T: accidentals in grace\r\n"
    <> "M: 2/4\r\n"
    <> "L: 1/16\r\n"
    <> "K: Gm\r\n"
    <> "{Bc=B}=c |\r\n"
    
chordAccidental :: String
chordAccidental =
    "X:1\r\n"
    <> "T: spacing\r\n"
    <> "M: 3/4\r\n"
    <> "L: 1/8\r\n"
    <> "K: Dm\r\n"
    <> " [B^g] |\r\n"    

main :: Effect Unit
main = do
  _ <- log $ debugFirstBar chordAccidental
  pure unit
