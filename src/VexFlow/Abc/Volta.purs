module VexFlow.Abc.Volta
  ( VexVolta
  , startVolta
  , completeVolta
  , isMidVolta
  , isEndVolta
  ) where

-- | support for Voltas (repeated sections that alter with each iteration)
-- | these are simply introduced in ABC by optional volta defintion number(s) but
-- | in VexFlow need to be extended over all the bars in which they have an
-- | effect

import Prelude (($), (/=), (&&), show)
import Data.Abc (BarLine, Thickness(..))
import Data.Maybe (Maybe(..), isJust)

{- from VexFlow StaveVolta
export class Volta extends StaveModifier {
  static get type() {
    return {
      NONE: 1,
      BEGIN: 2,
      MID: 3,
      END: 4,
      BEGIN_END: 5,
    };
  }
-}

-- | the Volta (repeat count) data type as passed to VexFlow
-- | we can't really use enumerated types here because of JavaScript interop
type VexVolta =
  { voltaType :: Int     -- 1 .. 5 (see above)
  , iteration :: String  -- Ist or 2nd iteration or "" if mid-volta
  }

-- | build a beginning Volta definition for a bar. When beginning, we only
-- | recognize a new Begin Volta or a continuation of a previous bar's Volta
-- | but the overall definition is incomplete.
startVolta :: BarLine -> Boolean -> Maybe VexVolta
startVolta barLine isCurrentlyMidVolta =
  case barLine.iteration of
    Nothing ->
      if (isCurrentlyMidVolta) then
        if (isEndVolta barLine) then
          Nothing
        else
          Just { voltaType : 3           -- Mid (continuation)
                , iteration : ""
                }
      else
        Nothing
    Just i ->
      Just { voltaType : 2               -- Begin
           , iteration : show i
           }

-- | We complete the Volta definition after the fact when we attempt to move any
-- | bar end marker back to the preceding bar.  This gives us an opportunity
-- | to set the Volta end charactersitics properly.
completeVolta :: Maybe VexVolta -> Maybe VexVolta
completeVolta mvolta =
  case mvolta of
    Nothing ->
      -- shouldn't happen
      Nothing
    Just volta ->
      let
        newVoltaType =
          case volta.voltaType of
            2 ->
              5                  -- Begin -> Begin_End
            3 ->
              4                  -- Mid -> End
            5 ->
              4                  -- Begin_End -> End
            x ->
              x
      in
        Just $ volta {voltaType = newVoltaType}

-- | detect whether the current bar is currently within a volta
-- | switched on by an iteration marker
-- | switched off by an end repeat
-- | otherwise unchanged
isMidVolta :: BarLine -> Boolean -> Boolean
isMidVolta barLine current =
  if (isJust barLine.iteration) then
    true
  else if (isEndVolta barLine) then
    false
  else
    current

-- | return true if the current bar indicates the end of a Volta section
-- | not entirely sure here what the rules should be.  We'll say a section ends
-- | if there is a Begin, End or BeginAndEnd repeat or if there is a thick
-- | barline
isEndVolta :: BarLine -> Boolean
isEndVolta barLine  =
  case barLine.iteration of 
     Nothing -> 
       (barLine.thickness /= Thin && barLine.thickness /= Invisible)
     Just volta -> 
       true
   
