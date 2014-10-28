module Language.Conversion.Con2Bin where

import           Control.Monad (msum)
import           Data.List (delete, sortBy, minimumBy)
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import           Language.POS (toXP)
import qualified Language.Structure.Binary       as Bin
import qualified Language.Structure.Constituency as Con
import qualified Language.Structure.Dependency   as Dep
import           Language.Word (Word (..))
import           Debug.Trace (traceShow)


-- |Convert dependency structures to binary constituency structures,
--  ensuring that only the minimal number of projections are made.
toledo :: Dep.Tree -> Con.Tree -> Bin.Tree
toledo _ (Con.Leaf word) = Bin.Leaf word
toledo dep (Con.Node _ children) = let

  gov :: Con.Tree
  gov =
    minimumBy (comparing (minimum . map (dependencyLevel dep) . Con.allWords)) children

  deps :: [Con.Tree]
  deps = delete gov children

  x      = Con.topMostPOS gov
  xp     = toXP x

  sorted :: [Con.Tree]
  sorted = sortBy (flip $ Con.nearestThenLeftMost (Con.leftMostIndex gov)) deps

  asbin  :: [Bin.Tree]
  asbin  = map (toledo dep) sorted

  asfunc :: Bin.Tree -> Bin.Tree
  asfunc = foldr ((.) . Bin.node xp) id asbin

  in traceShow sorted $ asfunc (toledo dep gov)


-- |Compute the depth of a word in the dependency tree.
dependencyLevel :: Dep.Tree -> Word -> Int
dependencyLevel dep (Word _ _ i) = fromMaybe maxBound (go 0 dep)
  where
    go :: Int -> Dep.Tree -> Maybe Int
    go n (Dep.Node (Word _ _ j) deps)
      = if i == j then Just n else msum (map (go (n+1)) deps)
