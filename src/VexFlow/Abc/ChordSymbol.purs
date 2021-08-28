module VexFlow.Abc.ChordSymbol 
  (attachChordSymbols) where

import Data.Array (foldl, modifyAt)
import Data.Maybe (Maybe(..))
import VexFlow.Types (ChordSymbol, MusicSpec(..), NoteSpec)

-- | Attach chord symbols to the notes that the precede, allowing VexFlow to position
-- | each symbol immediately above the note.  (In ABC, chord symbols are free-standing
-- | but come before the note they decorate.)
attachChordSymbols :: MusicSpec -> MusicSpec
attachChordSymbols (MusicSpec ms) =
  MusicSpec ms { noteSpecs = newNoteSpecs } 

  where
    newNoteSpecs = foldl attachChordSymbol ms.noteSpecs ms.chordSymbols 
 
attachChordSymbol :: Array NoteSpec -> ChordSymbol -> Array NoteSpec 
attachChordSymbol  noteSpecs chordSymbol = 
  case (modifyAt chordSymbol.noteIndex (\ns -> ns {chordSymbol = chordSymbol.symbol}) noteSpecs) of 
    Nothing -> noteSpecs 
    Just newSpecs -> newSpecs
   