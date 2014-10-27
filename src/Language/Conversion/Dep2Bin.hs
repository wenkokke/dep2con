module Language.Conversion.Dep2Bin where

import           Data.List (sortBy)
import           Data.Monoid ((<>))
import           Language.POS (POS (..), toX', toXP)
import qualified Language.Structure.Binary       as Bin
import           Language.Structure.Dependency (leftMostIndex)
import qualified Language.Structure.Dependency   as Dep
import           Language.Word (Word (..))


-- |Convert dependency structures to binary constituency structures,
--  ensuring that only the minimal number of projections are made.
toledo :: Dep.Tree -> Bin.Tree
toledo (Dep.Node (Word "ROOT" (POS "ROOT") 0) [dep]) = toledo dep
toledo (Dep.Node gov []) = Bin.Leaf gov
toledo (Dep.Node gov deps)

  = Bin.Node xp (Bin.Leaf gov)
    . foldr1 (Bin.Node x')
    . map toledo
    . sortBy (nearestThenLeftMost (serial gov))
    $ deps

  where
    x  = pos gov
    x' = toX' x
    xp = toXP x


-- |Compute the minimum distance from any node in the (sub-)tree to a
--  specific serial. /O(n)/
minDistance :: Int -> Dep.Tree -> Int
minDistance i = minimum . map (abs . (i -) . serial) . Dep.allWords


-- |Compare two trees, first by their distance to their governor, and
--  second by their index in the sentence (from the left). /O(n+m)/
nearestThenLeftMost :: Int -> Dep.Tree -> Dep.Tree -> Ordering
nearestThenLeftMost i x y = byDistanceToGovernor <> byIndex
  where
    byDistanceToGovernor = minDistance i x `compare` minDistance i y
    byIndex              = leftMostIndex x `compare` leftMostIndex y
