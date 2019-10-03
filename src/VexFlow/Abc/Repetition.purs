module VexFlow.Abc.Repetition
  ( Repetition
  , buildRepetition) where

import Prelude ((==))


{- from VexFlow Staverepetition
export class Repetition extends StaveModifier {
  static get CATEGORY() { return 'repetitions'; }
  static get type() {
    return {
      NONE: 1,         // no coda or segno
      CODA_LEFT: 2,    // coda at beginning of stave
      CODA_RIGHT: 3,   // coda at end of stave
      SEGNO_LEFT: 4,   // segno at beginning of stave
      SEGNO_RIGHT: 5,  // segno at end of stave
      DC: 6,           // D.C. at end of stave
      DC_AL_CODA: 7,   // D.C. al coda at end of stave
      DC_AL_FINE: 8,   // D.C. al Fine end of stave
      DS: 9,           // D.S. at end of stave
      DS_AL_CODA: 10,  // D.S. al coda at end of stave
      DS_AL_FINE: 11,  // D.S. al Fine at end of stave
      FINE: 12,        // Fine at end of stave
    };
  }
-}

{- from the ABC spec v2.1
!trill!                "tr" (trill mark)
!trill(!               start of an extended trill
!trill)!               end of an extended trill
!lowermordent!         short /|/|/ squiggle with a vertical line through it
!uppermordent!         short /|/|/ squiggle
!mordent!              same as !lowermordent!
!pralltriller!         same as !uppermordent!
!roll!                 a roll mark (arc) as used in Irish music
!turn!                 a turn mark (also known as gruppetto)
!turnx!                a turn mark with a line through it
!invertedturn!         an inverted turn mark
!invertedturnx!        an inverted turn mark with a line through it
!arpeggio!             vertical squiggle
!>!                    > mark
!accent!               same as !>!
!emphasis!             same as !>!
!fermata!              fermata or hold (arc above dot)
!invertedfermata!      upside down fermata
!tenuto!               horizontal line to indicate holding note for full duration
!0! - !5!              fingerings
!+!                    left-hand pizzicato, or rasp for French horns
!plus!                 same as !+!
!snap!                 snap-pizzicato mark, visually similar to !thumb!
!slide!                slide up to a note, visually similar to a half slur
!wedge!                small filled-in wedge mark
!upbow!                V mark
!downbow!              squared n mark
!open!                 small circle above note indicating open string or harmonic
!thumb!                cello thumb symbol
!breath!               a breath mark (apostrophe-like) after note
!pppp! !ppp! !pp! !p!  dynamics marks
!mp! !mf! !f! !ff!     more dynamics marks
!fff! !ffff! !sfz!     more dynamics marks
!crescendo(!           start of a < crescendo mark
!<(!                   same as !crescendo(!
!crescendo)!           end of a < crescendo mark, placed after the last note
!<)!                   same as !crescendo)!
!diminuendo(!          start of a > diminuendo mark
!>(!                   same as !diminuendo(!
!diminuendo)!          end of a > diminuendo mark, placed after the last note
!>)!                   same as !diminuendo)!
!segno!                2 ornate s-like symbols separated by a diagonal line
!coda!                 a ring with a cross in it
!D.S.!                 the letters D.S. (=Da Segno)
!D.C.!                 the letters D.C. (=either Da Coda or Da Capo)
!dacoda!               the word "Da" followed by a Coda sign
!dacapo!               the words "Da Capo"
!fine!                 the word "fine"
!shortphrase!          vertical line on the upper part of the staff
!mediumphrase!         same, but extending down to the centre line
!longphrase!           same, but extending 3/4 of the way down

-}


-- | the Repetitiom data type
-- | we can't really use enumerated types here because of JavaScript interop
type Repetition =
  { repetitionType :: Int     -- 1 .. 12 (see above)
  , isLeft :: Boolean         -- repetitionLeft or repetitionRight
  }

-- | Build the repetition (if it exists) from the decoration and its position
-- | in the bar. note: decorations are not yet type-safe in the ABC parser
buildRepetition :: Int -> String -> Repetition
buildRepetition noteIndex decoration  =
  let
    isLeft = noteIndex == 0
  in
    case decoration of
      "segno" ->
         case isLeft of
           true  ->  { repetitionType: 4, isLeft }
           false -> { repetitionType: 5, isLeft }
      "coda" ->
         case isLeft of
           true  ->  { repetitionType: 2, isLeft }
           false -> { repetitionType: 3, isLeft }
      "D.S." ->   { repetitionType: 9, isLeft }
      "D.C." ->   { repetitionType: 6, isLeft }
      "dacoda" -> { repetitionType: 7, isLeft }
      "dacopo" -> { repetitionType: 8, isLeft }
      "fine" ->   { repetitionType: 12, isLeft }
      _ -> { repetitionType: 1, isLeft }
