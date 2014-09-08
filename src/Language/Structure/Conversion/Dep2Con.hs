module Language.Structure.Conversion.Dep2Con where

import           Language.POS (POS, toP)
import qualified Language.Structure.Constituency as Con
import qualified Language.Structure.Dependency as Dep
import           Language.Word (Word (..))

{-
xBar :: Dep.Tree -> Con.Tree
xBar (Dep.Node gov deps) = _


collins :: (Word -> POS) -> Dep.Tree -> Con.Tree
collins tag (Dep.Node word [ ])
  = Con.Node (tag word) [Con.Leaf word]
collins tag (Dep.Node word (dep : deps))
  = Con.Node (toP (tag' dep')) (dep' : word' : deps')
  where
    tag' :: Con.Tree -> POS
    tag' (Con.Leaf word) = tag word
    tag' (Con.Node pos _) = pos
    dep'   :: Con.Tree
    dep'   = collins tag . unLink $ dep
    word'  :: Con.Tree
    word'  = Con.Node (tag word) [Con.Leaf word]
    deps'  :: [Con.Tree]
    deps'  = map (collins tag . Dep.dependent) deps
-}
