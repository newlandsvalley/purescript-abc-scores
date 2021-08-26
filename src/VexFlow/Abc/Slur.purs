module VexFlow.Abc.Slur
  ( VexCurve
  , VexCurves
  , SlurBracket(..)
  , vexCurves
  ) where

-- | Slur representation in terms of balanced left and right brackets
-- | within a bar together with their note indices

import Data.Array (cons, reverse)
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (class Eq, class Show, (<>), show)

-- | Minimal VexFlow description of a Curve which represents a slur
-- | where from and to are indexes into the note array in a bar
type VexCurve =
  { from :: Int
  , to :: Int
  }

type VexCurves = Array VexCurve

-- | A Slur Bracket
data SlurBracket
  = LeftBracket Int -- note index at '('
  | RightBracket Int -- note index at ')'

derive instance eqSlurBracket :: Eq SlurBracket

instance showSlurBracket :: Show SlurBracket where
  show (LeftBracket i) = "L:" <> show i
  show (RightBracket i) = "R:" <> show i

data SlurStack = SlurStack (List SlurBracket) VexCurves

-- | balance any left and right brackets and generate the VexFlow curves
-- | that represent slurs from a left to a right note index
vexCurves :: Array SlurBracket -> VexCurves
vexCurves brackets =
  let
    (SlurStack _ curves) = -- balance brackets
      foldl push empty brackets
  in
    reverse curves

-- | the empty stack
empty :: SlurStack
empty = SlurStack Nil []

-- | push the next bracket onto the stack, removing any
-- | balanced left and right brackets to the curves array
push :: SlurStack -> SlurBracket -> SlurStack
push slurStack new =
  case new of
    RightBracket to ->
      let
        Tuple head (SlurStack stack curves) = pop slurStack
      in
        case head of
          (Just (LeftBracket from)) ->
            SlurStack stack (cons { from, to } curves)
          (Just rightBracket) ->
            SlurStack (new : rightBracket : stack) curves
          Nothing ->
            SlurStack (new : stack) curves
    leftBracket ->
      let
        (SlurStack stack curves) = slurStack
      in
        SlurStack (leftBracket : stack) curves

-- | pop a bracket from the stack
pop :: SlurStack -> Tuple (Maybe SlurBracket) SlurStack
pop (SlurStack Nil curves) = Tuple Nothing (SlurStack Nil curves)
pop (SlurStack (x : xs) curves) = Tuple (Just x) (SlurStack xs curves)
