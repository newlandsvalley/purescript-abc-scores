module VexFlow.Abc.Volta
  ( Volta
  , startVolta
  , completeVolta
  , isMidVolta
  ) where

-- | support for Voltas (repeated sections that alter with each iteration)
-- | these are simply introduced in ABC by an optional iteration number but
-- | in VexFlow need to be extended over all the bars in which they have an
-- | effect

import Prelude (($), (==), (/=), (&&), (||), show)
import Data.Abc (BarType, Repeat(..), Thickness(..))
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

-- | the Volta (repeat count) data type
-- | we can't really use enumerated types here because of JavaScript interop
type Volta =
  { voltaType :: Int     -- 1 .. 5 (see above)
  , iteration :: String  -- Ist or 2nd iteration or "" if mid-volta
  }

-- | build a beginning Volta definition for a bar. When beginning, we only
-- | recognize a new Begin Volta or a continuation of a previous bar's Volta
-- | but the overall definition is incomplete.
startVolta :: BarType -> Boolean -> Boolean -> Maybe Volta
startVolta barType isEmptyBar isCurrentlyMidVolta =
  case barType.iteration of
    Nothing ->
      if (isCurrentlyMidVolta) then
        if (isEndVolta barType isEmptyBar) then
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
completeVolta :: Maybe Volta -> Maybe Volta
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
              5                  -- Begin -> begin_End
            3 ->
              4                  -- Mid -> End
            x ->
              x
      in
        Just $ volta {voltaType = newVoltaType}

-- | detect whether the current bar is currently within a volta
-- | switched on by an iteration marker
-- | switched off by an end repeat
-- | otherwise unchanged
isMidVolta :: BarType -> Boolean -> Boolean -> Boolean
isMidVolta barType isEmptyBar current =
  if (isJust barType.iteration) then
    true
  else if (isEndVolta barType isEmptyBar) then
    false
  else
    current

-- | THIS IS HACKY
-- | return true if the current bar indicates the end of a Volta section
-- | not entirely sure here what the rules should be.  We'll say a section ends
-- | if there is a Begin, End or BeginAndEnd repeat or if there is a thick
-- | barline or else if the bar itself is devoid of contents
isEndVolta :: BarType -> Boolean -> Boolean
isEndVolta barType isEmptyBar =
     (barType.repeat == Just End)
  || (barType.repeat == Just BeginAndEnd)
  || (barType.repeat == Just Begin)
  || (barType.thickness /= Thin && barType.thickness /= Invisible)
  || isEmptyBar
