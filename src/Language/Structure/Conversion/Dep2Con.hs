module Language.Structure.Conversion.Dep2Con where

import           Data.List (sort)
import           Language.POS (POS(POS), toXP, toX')
import qualified Language.Structure.Constituency as Con
import           Language.Structure.Dependency (dependent)
import qualified Language.Structure.Dependency as Dep
import           Language.Word (Word(Word))


isSpec, isArg, isMod :: POS -> Dep.Link -> Bool
isSpec = undefined
isArg  = undefined
isMod  = undefined

(<:) :: [a] -> a -> [a]
xs <: x = xs ++ [x]


-- |Convert dependency structures to constituency structures,
--  remaining faithful to x-bar theory to the fullest extend.
covington :: (Word -> POS) -> Dep.Tree -> Con.Tree
covington tag (Dep.Node gov deps) =
  Con.Node xp
    ( map (covington tag . dependent) specs
    <: Con.Node x'
      ( map (covington tag . dependent) mods
      <: Con.Node x'
        ( map (covington tag . dependent) args
        <: Con.Node x
          [
            Con.Leaf gov
          ]
        )
      )
    )
  where
    xp    = toXP x
    x'    = toX' x
    x     = tag gov
    specs = filter (isSpec x) deps
    mods  = filter (isMod x) deps
    args  = filter (isArg x) deps


-- |Convert dependency structures to constituency structures,
--  ensuring that only the minimal number of projections are made.
collins :: (Word -> POS) -> Dep.Tree -> Con.Tree
collins tag (Dep.Node (Word "ROOT" 0) deps)
  = Con.Node (POS "ROOT") (map (collins tag . dependent) deps)
collins tag (Dep.Node gov [])
  = Con.Node (tag gov) [Con.Leaf gov]
collins tag (Dep.Node gov deps)
  = Con.Node xp (sort (gov' : deps'))
  where
    xp    = toXP x
    x     = tag gov
    gov'  :: Con.Tree
    gov'  = Con.Node x [Con.Leaf gov]
    deps' :: [Con.Tree]
    deps' = map (collins tag . dependent) deps
