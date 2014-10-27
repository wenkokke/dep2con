module Language.Conversion.Dep2Bin where

import           Data.Foldable (foldMap)
import           Data.List (sortBy)
import           Data.Monoid ((<>))
import           Data.Ord (comparing)
import           Language.POS (POS (..), toX', toXP)
import qualified Language.Structure.Binary       as Bin
import qualified Language.Structure.Dependency   as Dep
import           Language.Word (Word (..))


-- |Convert dependency structures to binary constituency structures,
--  ensuring that only the minimal number of projections are made.
toledo :: Dep.Tree -> Bin.Tree
toledo (Dep.Node (Word "ROOT" (POS "ROOT") 0) [dep]) = toledo dep
toledo (Dep.Node gov []) = Bin.Leaf gov
toledo (Dep.Node gov deps) = let

  x      = pos gov
  xp     = toXP x

  sorted :: [Dep.Tree]
  sorted = sortBy (flip $ nearestThenLeftMost (serial gov)) deps

  asbin  :: [Bin.Tree]
  asbin  = map toledo sorted

  asfunc :: Bin.Tree -> Bin.Tree
  asfunc = foldr ((.) . mkNode xp) id asbin

  in asfunc (Bin.Leaf gov)


-- |Compute a node with the right left/right.
mkNode :: POS -> Bin.Tree -> Bin.Tree -> Bin.Tree
mkNode pos x y =
  if Bin.leftMostIndex x < Bin.leftMostIndex y
  then Bin.Node pos x y
  else Bin.Node pos y x


-- |Compute the minimum distance from any node in the (sub-)tree to a
--  specific serial. /O(n)/
minDistance :: Int -> Dep.Tree -> Int
minDistance i = minimum . map (abs . (i -) . serial) . Dep.allWords


-- |Compare two trees, first by their distance to their governor, and
--  second by their index in the sentence (from the left). /O(n+m)/
nearestThenLeftMost :: Int -> Dep.Tree -> Dep.Tree -> Ordering
nearestThenLeftMost i x y = byDistanceToGovernor <> byIndex
  where
    byDistanceToGovernor = comparing (minDistance i) x y
    byIndex              = comparing Dep.leftMostIndex x y
