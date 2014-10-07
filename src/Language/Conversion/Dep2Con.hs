module Language.Structure.Conversion.Dep2Con where

import           Data.List (insert)
import           Language.POS (POS (..), toXP)
import qualified Language.Structure.Constituency as Con
import qualified Language.Structure.Dependency as Dep
import           Language.Word (Word (Word,pos))


-- |Convert dependency structures to constituency structures,
--  ensuring that only the minimal number of projections are made.
collins :: Dep.Tree -> Con.Tree
collins (Dep.Node (Word "ROOT" (POS "ROOT") 0) deps)
  = Con.Node (POS "ROOT") (map collins deps)
collins (Dep.Node gov [])
  = Con.Node (pos gov) [Con.Leaf gov]
collins (Dep.Node gov deps)
  = Con.Node xp (insert gov' deps')
  where
    x     = pos gov
    xp    = toXP x
    gov'  = Con.Node x [Con.Leaf gov]
    deps' = map collins deps
