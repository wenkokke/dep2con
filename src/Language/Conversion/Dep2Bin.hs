module Language.Conversion.Dep2Bin where

import           Data.List (sortBy)
import           Language.POS (POS (..), toXP)
import qualified Language.Structure.Binary     as Bin
import qualified Language.Structure.Dependency as Dep
import           Language.Word (Word (..))


-- |Convert dependency structures to binary constituency structures,
--  ensuring that only the minimal number of projections are made.
collinsToledo :: Dep.Tree -> Bin.Tree
collinsToledo (Dep.Node (Word "ROOT" (POS "ROOT") 0) [dep]) = collinsToledo dep
collinsToledo (Dep.Node gov [])   = Bin.Leaf gov
collinsToledo (Dep.Node gov deps) = let

  x      = pos gov
  xp     = toXP x

  sorted :: [Dep.Tree]
  sorted = sortBy (flip $ Dep.nearestThenLeftMost (serial gov)) deps

  asbin  :: [Bin.Tree]
  asbin  = map collinsToledo sorted

  asfunc :: Bin.Tree -> Bin.Tree
  asfunc = foldr ((.) . Bin.node xp) id asbin

  in asfunc (Bin.Leaf gov)
